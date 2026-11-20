module Oglyad.Source.Cache
  ( cacheDir,
    loadJson,
    saveJson,
    CacheEntry (..),
    AndaEntry (..),
    UpstreamCache,
    lookupFresh,
  )
where

import Control.Exception (SomeException, finally, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import GHC.Generics (Generic)
import Oglyad.Types (SubmoduleEvent, UpstreamSource)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "oglyad"

loadJson :: forall a. (FromJSON a) => FilePath -> IO (Maybe a)
loadJson name = do
  path <- (</> name) <$> cacheDir
  r <- try (eitherDecodeFileStrict path) :: IO (Either SomeException (Either String a))
  pure (either (const Nothing) (either (const Nothing) Just) r)

-- | a unique temp file renamed into place, so a ctrl-c or a second run
-- writing at the same time can't leave a half-written cache behind
saveJson :: (ToJSON a) => FilePath -> a -> IO ()
saveJson name x = bestEffort $ do
  dir <- cacheDir
  createDirectoryIfMissing True dir
  (tmp, h) <- openBinaryTempFile dir (name <> ".tmp")
  LBS.hPut h (encode x) `finally` hClose h
  renameFile tmp (dir </> name)

bestEffort :: IO () -> IO ()
bestEffort action = either (\(_ :: SomeException) -> ()) id <$> try action

data CacheEntry = CacheEntry
  { ceFetched :: UTCTime,
    ceResult :: Either Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AndaEntry = AndaEntry
  { aeFetched :: UTCTime,
    aeResult :: Either Text Text,
    aeEvents :: [SubmoduleEvent]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | keyed by the rendered 'UpstreamSource' so json keys stay plain strings
type UpstreamCache = Map Text CacheEntry

lookupFresh :: NominalDiffTime -> UTCTime -> UpstreamSource -> UpstreamCache -> Maybe CacheEntry
lookupFresh maxAge now src c = do
  e <- Map.lookup (T.pack (show src)) c
  if diffUTCTime now (ceFetched e) <= maxAge then Just e else Nothing
