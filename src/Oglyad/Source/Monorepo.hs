-- | reads a local checkout of terrapkg/packages
module Oglyad.Source.Monorepo
  ( PkgDir (..)
  , SpecInfo (..)
  , findPackageDirs
  , hclString
  , querySpec
  , lastCommits
  , headCommit
  , specFingerprint
  , macroFingerprint
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, foldM, forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Oglyad.Version (EVR, parseEVR)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.Process.Typed (proc, readProcess)

-- | a directory with an anda.hcl in it
data PkgDir = PkgDir
  { pdPath :: FilePath -- ^ relative to the monorepo root
  , pdSpec :: Maybe FilePath -- ^ relative to the monorepo root
  , pdSubrepo :: Maybe Text -- ^ `subrepo = "extras"` from the labels block
  , pdUpdateRhai :: Maybe Text -- ^ file contents
  }
  deriving stock (Eq, Show)

data SpecInfo = SpecInfo
  { siName :: Text
  , siEVR :: EVR
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | walk anda/ and return every project directory
findPackageDirs :: FilePath -> IO [PkgDir]
findPackageDirs root = go "anda"
  where
    go rel = do
      let abs' = root </> rel
      entries <- listDirectory abs'
      hasHcl <- doesFileExist (abs' </> "anda.hcl")
      here <- if hasHcl then pure <$> readPkgDir rel entries else pure []
      subdirs <- filterM (doesDirectoryExist . (abs' </>)) [e | e <- entries, take 1 e /= "."]
      nested <- concat <$> forM subdirs (go . (rel </>))
      pure (here <> nested)

    readPkgDir rel entries = do
      let abs' = root </> rel
      hcl <- TIO.readFile (abs' </> "anda.hcl")
      let fromHcl = T.unpack <$> hclString "spec" hcl
          fallback = listToMaybe [e | e <- entries, takeExtension e == ".spec"]
      hasRhai <- doesFileExist (abs' </> "update.rhai")
      rhai <- if hasRhai then Just <$> TIO.readFile (abs' </> "update.rhai") else pure Nothing
      pure
        PkgDir
          { pdPath = rel
          , pdSpec = (rel </>) <$> maybe fallback Just fromHcl
          , pdSubrepo = hclString "subrepo" hcl
          , pdUpdateRhai = rhai
          }

-- | pull the first `key = "value"` out of anda.hcl without a real HCL parser.
-- every anda.hcl in the monorepo is simple enough for this to hold up
hclString :: Text -> Text -> Maybe Text
hclString key hcl =
  listToMaybe
    [ T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') rest))
    | l <- map T.strip (T.lines hcl)
    , Just rest <- [T.stripPrefix key l]
    , "=" `T.isPrefixOf` T.stripStart rest
    , "\"" `T.isInfixOf` rest
    ]

-- | let rpm evaluate the spec for us. hand-parsing specs means reimplementing
-- the macro engine, which is a terrible idea
querySpec :: Text -> FilePath -> IO (Either Text SpecInfo)
querySpec dist spec = do
  let qf = "%{NAME}\t%|EPOCH?{%{EPOCH}}:{0}|:%{VERSION}-%{RELEASE}\n"
  (code, out, err) <-
    readProcess . proc "rpmspec" $
      [ "-q", "--srpm"
      , "--define", "dist " <> T.unpack dist
      -- some specs load macro files from their own dir via %{_sourcedir}
      , "--define", "_sourcedir " <> takeDirectory spec
      , "--qf", qf, spec
      ]
  pure $ case code of
    ExitFailure _ -> Left (T.strip (decode err))
    ExitSuccess -> case T.splitOn "\t" <$> take 1 (T.lines (decode out)) of
      [[name, evr]] | Just e <- parseEVR evr -> Right (SpecInfo name e)
      _ -> Left ("unexpected rpmspec output: " <> decode out)
  where
    decode = TE.decodeUtf8Lenient . LBS.toStrict

-- | time of the last commit touching each package dir, from a single
-- `git log` over the whole history.
--
-- asking git once per directory (`git log -1 -- dir`) makes it walk and diff
-- history from the top every time, which for 1300 dirs and 20k commits burned
-- ~25s of cpu. one pass listing every changed file per commit takes ~0.5s,
-- and since git lists newest commits first, the first time we see a path
-- under a dir is that dir's last commit
lastCommits :: FilePath -> [FilePath] -> IO (Map FilePath UTCTime)
lastCommits root dirs = do
  (code, out, _) <-
    readProcess . proc "git" $
      ["-C", root, "-c", "core.quotePath=false", "log", "--format=@%ct", "--name-only", "--no-renames", "HEAD", "--", "anda"]
  pure $ case code of
    ExitFailure _ -> Map.empty
    ExitSuccess -> go Nothing Map.empty (T.lines (TE.decodeUtf8Lenient (LBS.toStrict out)))
  where
    wanted = Set.fromList (map T.pack dirs)
    total = Set.size wanted
    go _ acc _ | Map.size acc == total = acc -- every dir seen, stop reading
    go _ acc [] = acc
    go cur acc (l : ls) = case T.stripPrefix "@" l of
      -- unix seconds. iso dates would need care since git writes UTC as "Z",
      -- which the ZonedTime parser rejects
      Just stamp -> go (posixSecondsToUTCTime . fromInteger . fst <$> either (const Nothing) Just (TR.decimal stamp)) acc ls
      Nothing -> case cur of
        Just t | not (T.null l) -> go cur (foldl' (claim t) acc (ancestors l)) ls
        _ -> go cur acc ls
    -- a file counts for every package dir above it, like `git log -- dir` does
    claim t acc d
      | d `Set.member` wanted, not (Map.member (T.unpack d) acc) = Map.insert (T.unpack d) t acc
      | otherwise = acc
    ancestors f = case T.breakOnEnd "/" f of
      ("", _) -> []
      (parentSlash, _) -> let parent = T.dropEnd 1 parentSlash in parent : ancestors parent

-- | the commit HEAD points at, if this is a git checkout
headCommit :: FilePath -> IO (Maybe Text)
headCommit root = do
  (code, out, _) <- readProcess (proc "git" ["-C", root, "rev-parse", "HEAD"])
  pure $ case code of
    ExitSuccess -> Just (T.strip (TE.decodeUtf8Lenient (LBS.toStrict out)))
    ExitFailure _ -> Nothing

-- | a hash of everything that can change what rpmspec says about a package:
-- the files in its directory (spec, patches, macro files it loads), the dist
-- tag, and the installed rpm macros (see 'macroFingerprint')
specFingerprint :: Text -> Text -> FilePath -> FilePath -> IO Text
specFingerprint macros dist root rel = do
  let dir = root </> rel
  entries <- sort <$> listDirectory dir
  files <- filterM (doesFileExist . (dir </>)) entries
  let header = TE.encodeUtf8 (macros <> "\0" <> dist <> "\0" <> T.pack rel <> "\0")
  -- feed each file to the hash as its own chunk instead of concatenating
  -- them, which would copy every byte of the package dir once more
  ctx <- foldM (\c f -> do
    body <- BS.readFile (dir </> f)
    pure $! SHA256.updates c [TE.encodeUtf8 (T.pack f), "\0", body, "\0"]) (SHA256.update SHA256.init header) files
  pure (TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))

-- | fingerprint of the system's rpm macro setup (names, sizes and mtimes of
-- the macro files), so updating anda-srpm-macros or rpm itself invalidates
-- every cached spec evaluation
macroFingerprint :: IO Text
macroFingerprint = do
  let dirs = ["/usr/lib/rpm/macros.d", "/etc/rpm"]
      single = ["/usr/lib/rpm/macros", "/usr/lib/rpm/rpmrc", "/usr/bin/rpmspec"]
  listed <- fmap concat . forM dirs $ \d -> do
    r <- try (listDirectory d) :: IO (Either SomeException [FilePath])
    pure (map (d </>) (either (const []) sort r))
  stamps <- forM (single <> listed) $ \f -> do
    r <- try ((,) <$> getModificationTime f <*> getFileSize f) :: IO (Either SomeException (UTCTime, Integer))
    pure (f <> " " <> either (const "-") show r)
  pure (TE.decodeUtf8 (B16.encode (SHA256.hash (TE.encodeUtf8 (T.pack (unlines stamps))))))
