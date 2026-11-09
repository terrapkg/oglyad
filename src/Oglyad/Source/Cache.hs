-- | small json caches under $XDG_CACHE_HOME/oglyad, so reruns skip work that
-- can't have changed: spec evaluation, repodata parsing and upstream lookups
module Oglyad.Source.Cache
  ( loadJson,
    saveJson,
    CacheEntry (..),
    UpstreamCache,
    lookupFresh,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import GHC.Generics (Generic)
import Oglyad.Types (UpstreamSource)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath ((</>))

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "oglyad"

-- | a missing or corrupt cache file is just a cache miss
loadJson :: forall a. (FromJSON a) => FilePath -> IO (Maybe a)
loadJson name = do
  path <- (</> name) <$> cacheDir
  r <- try (eitherDecodeFileStrict path) :: IO (Either SomeException (Either String a))
  pure (either (const Nothing) (either (const Nothing) Just) r)

-- | write to a temp file then rename, so a ctrl-c mid-write can't corrupt it
saveJson :: (ToJSON a) => FilePath -> a -> IO ()
saveJson name x = do
  dir <- cacheDir
  createDirectoryIfMissing True dir
  let path = dir </> name
  encodeFile (path <> ".tmp") x
  renameFile (path <> ".tmp") path

data CacheEntry = CacheEntry
  { ceFetched :: UTCTime,
    ceResult :: Either Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | upstream answers, keyed by the rendered 'UpstreamSource' so json keys
-- stay plain strings
type UpstreamCache = Map Text CacheEntry

lookupFresh :: NominalDiffTime -> UTCTime -> UpstreamSource -> UpstreamCache -> Maybe CacheEntry
lookupFresh maxAge now src c = do
  e <- Map.lookup (T.pack (show src)) c
  if diffUTCTime now (ceFetched e) <= maxAge then Just e else Nothing
