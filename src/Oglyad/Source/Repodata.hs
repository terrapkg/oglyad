-- | reads published dnf repodata: repomd.xml → primary.xml.{zst,gz,xz} → packages
module Oglyad.Source.Repodata
  ( RpmEntry (..),
    fetchRepo,
    decompressAndParse,
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
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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
import Text.XML.Cursor (attribute, attributeIs, element, fromDocument, ($//), (&/))
import Text.XML.Stream.Parse (parseBytes)

-- | one binary (or source) rpm from primary.xml
-- | strict fields, filled with copied text (see 'finish' in 'parsePrimary'),
-- so a parsed entry never keeps the decompressed file alive
data RpmEntry = RpmEntry
  { rpmName :: !Text,
    rpmArch :: !Text,
    rpmEVR :: !EVR,
    -- | e.g. zed-0.190.0-1.fc44.src.rpm, absent on src rpms
    rpmSourceRpm :: !(Maybe Text),
    rpmBuildTime :: !UTCTime
  }
  deriving stock (Eq, Show)

-- | builds per source package for a repo, reusing the last parse when the
-- repo hasn't changed. repomd.xml is tiny, and the primary.xml href in it
-- embeds a checksum, so an unchanged href means unchanged contents
fetchRepo :: Text -> App (Either String (Map Text RepoBuild))
fetchRepo base = do
  let file = "repodata-" <> T.unpack (T.map (\c -> if isAlphaNum c then c else '_') base) <> ".json"
  repomd <- httpGetE (base <> "/repodata/repomd.xml")
  case first httpErr repomd >>= parseRepomd of
    Left err -> pure (Left err)
    Right href ->
      liftIO (loadJson file) >>= \case
        Just (cachedHref, builds) | cachedHref == href -> pure (Right builds)
        _ -> do
          note ("downloading " <> T.unpack base <> "/" <> T.unpack href)
          r <- fmap latestBySource <$> fetchPrimaryAt base href
          case r of
            Right builds -> liftIO (saveJson file (href, builds))
            Left _ -> pure ()
          pure r

-- | fetch and parse the primary.xml at a href from repomd.xml
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

-- | decompress a primary.xml by its file extension and parse it, streaming
-- from the decompressor's pipe so even Fedora's ~250MB file never sits in
-- memory.
--
-- decompression goes through the zstd, gzip and xz commands, which every
-- Fedora install has. Fedora doesn't package a Haskell zstd binding, and
-- oglyad should build from Fedora's own libraries
decompressAndParse :: Text -> LBS.ByteString -> IO (Either String [RpmEntry])
decompressAndParse href compressed
  | ".zst" `T.isSuffixOf` href = viaCommand "zstd" ["--decompress", "--stdout", "--quiet"]
  | ".gz" `T.isSuffixOf` href = viaCommand "gzip" ["--decompress", "--stdout", "--quiet"]
  | ".xz" `T.isSuffixOf` href = viaCommand "xz" ["--decompress", "--stdout", "--quiet"]
  | ".xml" `T.isSuffixOf` href = pure (parsePrimary compressed)
  | otherwise = pure (Left ("don't know how to decompress " <> T.unpack href))
  where
    -- feed the download to the command's stdin and parse its stdout as it
    -- arrives. a corrupt archive makes the command fail, which shows up as
    -- a parse error or a non-zero exit, never a crash
    viaCommand cmd args = do
      env <- childEnv <$> getEnvironment
      let cfg = setEnv env (setStdin (byteStringInput compressed) (setStdout createPipe (setStderr nullStream (proc cmd args))))
      r <- try $ withProcessTerm cfg $ \p -> do
        parsed <- parsePrimaryHandle (getStdout p)
        code <- waitExitCode p
        pure $ case (code, parsed) of
          (ExitFailure n, _) -> Left (cmd <> " exited with " <> show n <> " decompressing " <> T.unpack href)
          (ExitSuccess, _) -> parsed
      pure (either (\e -> Left (cmd <> ": " <> show (e :: SomeException))) id r)

repoNs, commonNs, rpmNs :: Text -> Name
repoNs n = Name n (Just "http://linux.duke.edu/metadata/repo") Nothing
commonNs n = Name n (Just "http://linux.duke.edu/metadata/common") Nothing
rpmNs n = Name n (Just "http://linux.duke.edu/metadata/rpm") Nothing

-- | find the location of primary.xml in repomd.xml.
-- this uses the xml-conduit Cursor api, which builds the whole document in
-- memory. that's fine for a 2KB file:
--   $//  = all descendants matching
--   &/   = children matching
--   >=>  = compose two axes (it's just Kleisli composition on lists)
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

-- | what we've seen so far of the <package> we're currently inside
data PkgAcc = PkgAcc
  { accName :: Text,
    accArch :: Text,
    accEVR :: Maybe EVR,
    accTime :: Maybe UTCTime,
    accSrpm :: Maybe Text
  }

emptyAcc :: PkgAcc
emptyAcc = PkgAcc "" "" Nothing Nothing Nothing

-- | which element's text we're currently collecting
data Capture = CapName | CapArch | CapSrpm | CapNone
  deriving stock (Eq)

data St = St
  { stAcc :: !PkgAcc,
    stCap :: !Capture,
    -- | finished packages, newest first
    stOut :: [RpmEntry],
    -- | open elements, nonzero at the end means a truncated file
    stDepth :: !Int
  }

-- | parse decompressed primary.xml from memory. the real runs stream from a
-- pipe with 'parsePrimaryHandle'. this one is for small inputs and tests.
--
-- unlike repomd.xml this file is big (Terra's is ~20MB, Fedora's is ~200MB)
-- so we never build a DOM. instead the XML is turned into a stream of events
-- (element opened, text, element closed, ...) and folded over with a small
-- state machine. memory use stays proportional to the results, not the file
parsePrimary :: LBS.ByteString -> Either String [RpmEntry]
parsePrimary bs =
  first (\e -> show (e :: SomeException)) (runConduit (sourceLazy bs .| parseBytes def .| foldlC stepPrimary st0))
    >>= finishPrimary

-- | parse primary.xml straight from a handle.
--
-- reading the handle chunk by chunk matters: handing the parser a lazy
-- ByteString instead lets the thunk evaluating it hold on to the start of the
-- stream, which keeps the whole decompressed file (250MB for Fedora) alive
-- until parsing ends
parsePrimaryHandle :: Handle -> IO (Either String [RpmEntry])
parsePrimaryHandle h = do
  r <- try (runConduit (sourceHandle h .| parseBytes def .| foldlC stepPrimary st0))
  pure (either (\e -> Left (show (e :: SomeException))) finishPrimary r)

st0 :: St
st0 = St emptyAcc CapNone [] 0

finishPrimary :: St -> Either String [RpmEntry]
finishPrimary final
  | stDepth final /= 0 = Left "primary.xml ends in the middle of an element, truncated download?"
  | otherwise = Right (reverse (stOut final))

-- | one xml event. tracks element depth so truncation can be detected
stepPrimary :: St -> Event -> St
stepPrimary cur ev = case ev of
  EventBeginElement _ _ -> step cur {stDepth = stDepth cur + 1} ev
  EventEndElement _ -> step cur {stDepth = stDepth cur - 1} ev
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

    -- the text we collected is sliced out of the parser's buffers, and a
    -- slice keeps its whole buffer alive. copying is what stops 75k Fedora
    -- entries from pinning the entire decompressed file in memory
    --
    -- the build time needs the same care. UTCTime's fields are lazy, so
    -- without a deep force it stays a thunk pointing at the xml attribute it
    -- came from, which pins that attribute's whole decoder chunk
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

-- | source package name from a srpm filename: strip ".src.rpm", then the last
-- two dash-separated fields (version and release)
srpmName :: Text -> Maybe Text
srpmName file = do
  base <- T.stripSuffix ".src.rpm" file
  let parts = T.splitOn "-" base
  if length parts < 3
    then Nothing
    else Just (T.intercalate "-" (take (length parts - 2) parts))

-- | collapse binary rpms down to one build per source package, keeping the
-- newest build time. this is what lets us line repodata up with the monorepo
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
