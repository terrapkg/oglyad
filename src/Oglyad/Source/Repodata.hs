-- | reads published dnf repodata: repomd.xml → primary.xml.{zst,gz,xz} → packages
module Oglyad.Source.Repodata
  ( RpmEntry (..),
    fetchPrimary,
    fetchRepo,
    parseRepomd,
    parsePrimary,
    srpmName,
    latestBySource,
  )
where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Conduit (foldlC, runConduit, sourceLazy, (.|))
import Control.Exception (SomeException, evaluate, try)
import Control.Monad ((>=>))
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
import Oglyad.App (App, httpGet, note)
import Oglyad.Source.Cache (loadJson, saveJson)
import Oglyad.Types (RepoBuild (..))
import Oglyad.Version (EVR (..))
import Text.XML (Name (..), def, parseLBS)
import Text.XML.Cursor (attribute, attributeIs, element, fromDocument, ($//), (&/))
import Text.XML.Stream.Parse (parseBytes)

-- | one binary (or source) rpm from primary.xml
data RpmEntry = RpmEntry
  { rpmName :: Text,
    rpmArch :: Text,
    rpmEVR :: EVR,
    -- | e.g. zed-0.190.0-1.fc44.src.rpm, absent on src rpms
    rpmSourceRpm :: Maybe Text,
    rpmBuildTime :: UTCTime
  }
  deriving stock (Eq, Show)

-- | builds per source package for a repo, reusing the last parse when the
-- repo hasn't changed. repomd.xml is tiny, and the primary.xml href in it
-- embeds a checksum, so an unchanged href means unchanged contents
fetchRepo :: Text -> App (Either String (Map Text RepoBuild))
fetchRepo base = do
  let file = "repodata-" <> T.unpack (T.map (\c -> if isAlphaNum c then c else '_') base) <> ".json"
  repomd <- httpGet (base <> "/repodata/repomd.xml")
  case parseRepomd repomd of
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

-- | fetch and parse primary.xml for a repo base url
fetchPrimary :: Text -> App (Either String [RpmEntry])
fetchPrimary base = do
  repomd <- httpGet (base <> "/repodata/repomd.xml")
  either (pure . Left) (fetchPrimaryAt base) (parseRepomd repomd)

fetchPrimaryAt :: Text -> Text -> App (Either String [RpmEntry])
fetchPrimaryAt base href = do
  compressed <- httpGet (base <> "/" <> href)
  case decompressorFor href of
    Left err -> pure (Left err)
    Right decompress -> do
      -- decompression is lazy, so a corrupt archive only explodes once
      -- the parser pulls on it. force the whole parse here so that
      -- explosion turns into a Left instead of crashing the program later
      r <- liftIO (try (evaluate (forceEntries (parsePrimary (decompress compressed)))))
      pure (either (\e -> Left (show (e :: SomeException))) id r)
  where
    forceEntries r = case r of
      Right es -> length es `seq` r
      Left _ -> r

decompressorFor :: Text -> Either String (LBS.ByteString -> LBS.ByteString)
decompressorFor href
  | ".zst" `T.isSuffixOf` href = Right Zstd.decompress
  | ".gz" `T.isSuffixOf` href = Right GZip.decompress
  | ".xz" `T.isSuffixOf` href = Right Lzma.decompress
  | ".xml" `T.isSuffixOf` href = Right id
  | otherwise = Left ("don't know how to decompress " <> T.unpack href)

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

-- | parse decompressed primary.xml.
--
-- unlike repomd.xml this file is big (Terra's is ~20MB, Fedora's is ~200MB)
-- so we never build a DOM. instead the XML is turned into a stream of events
-- (element opened, text, element closed, ...) and folded over with a small
-- state machine. memory use stays proportional to the results, not the file
parsePrimary :: LBS.ByteString -> Either String [RpmEntry]
parsePrimary bs = do
  final <-
    first (\e -> show (e :: SomeException)) $
      runConduit (sourceLazy bs .| parseBytes def .| foldlC step' (St emptyAcc CapNone [] 0))
  if stDepth final /= 0
    then Left "primary.xml ends in the middle of an element, truncated download?"
    else Right (reverse (stOut final))
  where
    step' st ev = case ev of
      EventBeginElement _ _ -> step st {stDepth = stDepth st + 1} ev
      EventEndElement _ -> step st {stDepth = stDepth st - 1} ev
      _ -> step st ev

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

    finish PkgAcc {..} = do
      evr <- accEVR
      t <- accTime
      if T.null accName then Nothing else Just (RpmEntry accName accArch evr accSrpm t)

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
