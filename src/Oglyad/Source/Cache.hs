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

import Control.Exception (SomeException, finally, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import GHC.Generics (Generic)
import Oglyad.Types (UpstreamSource)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "oglyad"

-- | a missing or corrupt cache file is just a cache miss
loadJson :: forall a. (FromJSON a) => FilePath -> IO (Maybe a)
loadJson name = do
  path <- (</> name) <$> cacheDir
  r <- try (eitherDecodeFileStrict path) :: IO (Either SomeException (Either String a))
  pure (either (const Nothing) (either (const Nothing) Just) r)

-- | write to a temp file then rename, so a ctrl-c mid-write can't corrupt it.
-- caching is best effort: if the write fails (full disk, read-only home) the
-- run carries on and just won't be faster next time
saveJson :: (ToJSON a) => FilePath -> a -> IO ()
saveJson name x = do
  r <- try $ do
    dir <- cacheDir
    createDirectoryIfMissing True dir
    let path = dir </> name
    -- a unique temp name, so two runs at once can't interleave their writes.
    -- rename is atomic, so readers see the old file or the new one, never half
    (tmp, h) <- openBinaryTempFile dir (name <> ".tmp")
    LBS.hPut h (encode x) `finally` hClose h
    renameFile tmp path
  case r of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()

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
