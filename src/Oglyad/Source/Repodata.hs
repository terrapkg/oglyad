module Oglyad.Source.Repodata
  ( RpmEntry (..),
    fetchRepo,
    decompressAndParse,
    parseMetalink,
    parseRepomd,
    parsePrimary,
    srpmName,
    latestBySource,
  )
where

import Conduit (foldlC, runConduit, sourceHandle, sourceLazy, (.|))
import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, try)
import Control.Monad ((<$!>), (>=>))
import Control.Monad.Reader (liftIO)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.XML.Types (Content (..), Event (..))
import Oglyad.App (App, HttpError (..), httpGetE, note)
import Oglyad.Process (childEnv)
import Oglyad.Source.Cache (loadJson, saveJson)
import Oglyad.Types (RepoBuild (..))
import Oglyad.Version (EVR (..))
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.Process.Typed (byteStringInput, createPipe, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, waitExitCode, withProcessTerm)
import Text.XML (Name (..), def, parseLBS)
import Text.XML.Cursor (attribute, attributeIs, content, element, fromDocument, ($/), ($//), (&/))
import Text.XML.Stream.Parse (parseBytes)

-- | strict fields with copied text (see 'finish' in 'parsePrimary'), so an
-- entry never keeps the decompressed file alive
data RpmEntry = RpmEntry
  { rpmName :: !Text,
    rpmArch :: !Text,
    rpmEVR :: !EVR,
    -- | absent on src rpms
    rpmSourceRpm :: !(Maybe Text),
    rpmBuildTime :: !UTCTime
  }
  deriving stock (Eq, Show)

-- | the primary.xml href in repomd.xml embeds a checksum, so an unchanged
-- href means the cached parse is still good
fetchRepo :: Text -> App (Either String (Map Text RepoBuild))
fetchRepo url = do
  located <- fetchRepomd url
  case located >>= \(base, repomd) -> (base,) <$> parseRepomd repomd of
    Left err -> pure (Left err)
    Right (base, href) -> do
      let file = "repodata-" <> T.unpack (T.map (\c -> if isAlphaNum c then c else '_') base) <> ".json"
      liftIO (loadJson file) >>= \case
        Just (cachedHref, builds) | cachedHref == href -> pure (Right builds)
        _ -> do
          note ("downloading " <> T.unpack base <> "/" <> T.unpack href)
          r <- fmap latestBySource <$> fetchPrimaryAt base href
          case r of
            Right builds -> liftIO (saveJson file (href, builds))
            Left _ -> pure ()
          pure r

-- | metalinks are read like dnf reads them: mirrors by preference, taking the
-- first repomd.xml whose sha256 the metalink lists, so a mirror that hasn't
-- synced yet is skipped
fetchRepomd :: Text -> App (Either String (Text, LBS.ByteString))
fetchRepomd url
  | "metalink" `T.isInfixOf` url =
      httpGetE url >>= \r -> case first httpErr r >>= parseMetalink of
        Left err -> pure (Left ("can't read metalink " <> T.unpack url <> ": " <> err))
        Right (_, []) -> pure (Left ("metalink " <> T.unpack url <> " lists no mirrors"))
        Right (hashes, mirrors) -> tryMirrors hashes ("no mirror in " <> T.unpack url) mirrors
  | otherwise = fmap (url,) . first httpErr <$> httpGetE (url <> "/repodata/repomd.xml")
  where
    tryMirrors _ lastErr [] = pure (Left lastErr)
    tryMirrors hashes _ (m : rest) = case T.stripSuffix "/repodata/repomd.xml" m of
      Nothing -> tryMirrors hashes ("metalink mirror " <> T.unpack m <> " isn't a repomd.xml") rest
      Just base ->
        httpGetE m >>= \case
          Left e -> tryMirrors hashes (httpErr e) rest
          Right body
            | null hashes || sha256 body `elem` hashes -> pure (Right (base, body))
            | otherwise -> tryMirrors hashes (T.unpack m <> " doesn't match the metalink's checksum, is the mirror behind?") rest
    sha256 = TE.decodeUtf8 . B16.encode . SHA256.hashlazy

-- | the hashes include alternates, recent repomd.xml versions mirrors may
-- still serve
parseMetalink :: LBS.ByteString -> Either String ([Text], [Text])
parseMetalink bs = do
  doc <- first show (parseLBS def bs)
  let cur = fromDocument doc
      hashes = cur $// (element (metalinkNs "hash") >=> attributeIs "type" "sha256") &/ content
      urls =
        [ (pref, T.strip u)
        | el <- cur $// element (metalinkNs "url"),
          let pref = case attribute "preference" el of
                p : _ | Right (n, "") <- TR.decimal p -> n
                _ -> 0 :: Int,
          u <- [T.concat (el $/ content)]
        ]
  Right (map T.strip hashes, map snd (sortOn (Down . fst) urls))

fetchPrimaryAt :: Text -> Text -> App (Either String [RpmEntry])
fetchPrimaryAt base href = do
  download <- httpGetE (base <> "/" <> href)
  case first httpErr download of
    Left err -> pure (Left err)
    Right compressed -> liftIO (decompressAndParse href compressed)

httpErr :: HttpError -> String
httpErr = \case
  RateLimited why -> T.unpack why
  HttpFailed why -> T.unpack why

-- | streams from the decompressor's pipe, so even Fedora's ~250MB file never
-- sits in memory. it shells out to zstd, gzip and xz because Fedora doesn't
-- package a Haskell zstd binding, and oglyad should build from Fedora's own
-- libraries
decompressAndParse :: Text -> LBS.ByteString -> IO (Either String [RpmEntry])
decompressAndParse href compressed
  | ".zst" `T.isSuffixOf` href = viaCommand "zstd"
  | ".gz" `T.isSuffixOf` href = viaCommand "gzip"
  | ".xz" `T.isSuffixOf` href = viaCommand "xz"
  | ".xml" `T.isSuffixOf` href = pure (parsePrimary compressed)
  | otherwise = pure (Left ("don't know how to decompress " <> T.unpack href))
  where
    -- a corrupt archive shows up as a parse error or a non-zero exit, never a
    -- crash
    viaCommand cmd = do
      env <- childEnv <$> getEnvironment
      let decompressToStdout = ["--decompress", "--stdout", "--quiet"]
          cfg = setEnv env (setStdin (byteStringInput compressed) (setStdout createPipe (setStderr nullStream (proc cmd decompressToStdout))))
      r <- try $ withProcessTerm cfg $ \p -> do
        parsed <- parsePrimaryHandle (getStdout p)
        code <- waitExitCode p
        pure $ case (code, parsed) of
          (ExitFailure n, _) -> Left (cmd <> " exited with " <> show n <> " decompressing " <> T.unpack href)
          (ExitSuccess, _) -> parsed
      pure (either (\e -> Left (cmd <> ": " <> show (e :: SomeException))) id r)

repoNs, commonNs, rpmNs, metalinkNs :: Text -> Name
repoNs n = Name n (Just "http://linux.duke.edu/metadata/repo") Nothing
commonNs n = Name n (Just "http://linux.duke.edu/metadata/common") Nothing
rpmNs n = Name n (Just "http://linux.duke.edu/metadata/rpm") Nothing
metalinkNs n = Name n (Just "http://www.metalinker.org/") Nothing

-- | builds the whole document in memory, which is fine for a 2KB file
parseRepomd :: LBS.ByteString -> Either String Text
parseRepomd bs = do
  doc <- first show (parseLBS def bs)
  let hrefs =
        fromDocument doc
          $// (element (repoNs "data") >=> attributeIs "type" "primary")
          &/ (element (repoNs "location") >=> attribute "href")
  case hrefs of
    (h : _) -> Right h
    [] -> Left "no primary entry in repomd.xml"

data PkgAcc = PkgAcc
  { accName :: Text,
    accArch :: Text,
    accEVR :: Maybe EVR,
    accTime :: Maybe UTCTime,
    accSrpm :: Maybe Text
  }

emptyAcc :: PkgAcc
emptyAcc = PkgAcc "" "" Nothing Nothing Nothing

data Capture = CapName | CapArch | CapSrpm | CapNone
  deriving stock (Eq)

data St = St
  { stAcc :: !PkgAcc,
    stCap :: !Capture,
    stOut :: [RpmEntry],
    stOpenElements :: !Int
  }

-- | for small inputs and tests. real runs stream with 'parsePrimaryHandle'.
-- primary.xml is too big for a DOM (Fedora's is ~200MB), so XML events are
-- folded over instead, keeping memory proportional to the results
parsePrimary :: LBS.ByteString -> Either String [RpmEntry]
parsePrimary bs =
  first (\e -> show (e :: SomeException)) (runConduit (sourceLazy bs .| parseBytes def .| foldlC stepPrimary st0))
    >>= finishPrimary

-- | reads chunk by chunk because a lazy ByteString would let its thunk hold
-- on to the start of the stream, keeping the whole decompressed file alive
-- until parsing ends
parsePrimaryHandle :: Handle -> IO (Either String [RpmEntry])
parsePrimaryHandle h = do
  r <- try (runConduit (sourceHandle h .| parseBytes def .| foldlC stepPrimary st0))
  pure (either (\e -> Left (show (e :: SomeException))) finishPrimary r)

st0 :: St
st0 = St emptyAcc CapNone [] 0

finishPrimary :: St -> Either String [RpmEntry]
finishPrimary final
  | stOpenElements final /= 0 = Left "primary.xml ends in the middle of an element, truncated download?"
  | otherwise = Right (reverse (stOut final))

stepPrimary :: St -> Event -> St
stepPrimary cur ev = case ev of
  EventBeginElement _ _ -> step cur {stOpenElements = stOpenElements cur + 1} ev
  EventEndElement _ -> step cur {stOpenElements = stOpenElements cur - 1} ev
  _ -> step cur ev
  where
    step st = \case
      EventBeginElement name attrs
        | name == commonNs "package" -> st {stAcc = emptyAcc, stCap = CapNone}
        | name == commonNs "name" -> st {stCap = CapName}
        | name == commonNs "arch" -> st {stCap = CapArch}
        | name == rpmNs "sourcerpm" -> st {stCap = CapSrpm}
        | name == commonNs "version" ->
            let attr k = lookupAttr k attrs
                evr = EVR <$> (attr "epoch" >>= readInt) <*> attr "ver" <*> attr "rel"
             in st {stAcc = (stAcc st) {accEVR = evr}}
        | name == commonNs "time" ->
            let t = posixSecondsToUTCTime . fromIntegral <$> (lookupAttr "build" attrs >>= readInt)
             in st {stAcc = (stAcc st) {accTime = t}}
        | otherwise -> st
      EventContent (ContentText t) -> case stCap st of
        CapName -> st {stAcc = (stAcc st) {accName = accName (stAcc st) <> t}}
        CapArch -> st {stAcc = (stAcc st) {accArch = accArch (stAcc st) <> t}}
        CapSrpm -> st {stAcc = (stAcc st) {accSrpm = Just (maybe t (<> t) (accSrpm (stAcc st)))}}
        CapNone -> st
      EventEndElement name
        | name == commonNs "package" -> case finish (stAcc st) of
            Just e -> st {stOut = e : stOut st, stCap = CapNone}
            Nothing -> st {stCap = CapNone}
        | otherwise -> st {stCap = CapNone}
      _ -> st

    -- collected text is a slice of the parser's buffers and keeps the whole
    -- buffer alive, so it's copied. UTCTime's fields are lazy, so the build
    -- time is deep forced for the same reason
    finish PkgAcc {..} = do
      EVR e v r <- accEVR
      t <- accTime
      if T.null accName
        then Nothing
        else t `deepseq` (Just $! RpmEntry (T.copy accName) (T.copy accArch) (EVR e (T.copy v) (T.copy r)) (T.copy <$!> accSrpm) t)

    lookupAttr :: Name -> [(Name, [Content])] -> Maybe Text
    lookupAttr k attrs = do
      cs <- lookup k attrs
      pure (T.concat (mapMaybe contentText cs))

    contentText = \case
      ContentText t -> Just t
      ContentEntity _ -> Nothing

    readInt :: Text -> Maybe Int
    readInt t = case TR.decimal t of
      Right (n, "") -> Just n
      _ -> Nothing

srpmName :: Text -> Maybe Text
srpmName file = do
  base <- T.stripSuffix ".src.rpm" file
  let parts = T.splitOn "-" base
  if length parts < 3
    then Nothing
    else Just (T.intercalate "-" (take (length parts - 2) parts))

latestBySource :: [RpmEntry] -> Map Text RepoBuild
latestBySource = foldr step Map.empty
  where
    step e acc = case srcOf e of
      Nothing -> acc
      Just src -> Map.insertWith newer src (RepoBuild (srcEVR e) (rpmBuildTime e)) acc
    srcOf e = maybe (Just (rpmName e)) srpmName (rpmSourceRpm e)
    newer a b = if rbBuildTime a >= rbBuildTime b then a else b
    -- binary subpackages can carry their own Version/Release, so prefer the
    -- EVR spelled out in the srpm filename when there is one
    srcEVR e = case rpmSourceRpm e >>= srpmEVR of
      Just evr -> evr {evrEpoch = evrEpoch (rpmEVR e)}
      Nothing -> rpmEVR e
    srpmEVR file = do
      base <- T.stripSuffix ".src.rpm" file
      let parts = T.splitOn "-" base
      case reverse parts of
        rel : ver : _ : _ -> Just (EVR 0 ver rel)
        _ -> Nothing
