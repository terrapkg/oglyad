-- | reads a local checkout of terrapkg/packages
module Oglyad.Source.Monorepo
  ( PkgDir (..),
    SpecInfo (..),
    findPackageDirs,
    hclString,
    querySpec,
    lastCommits,
    headCommit,
    specFingerprint,
    macroFingerprint,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, foldM, forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Oglyad.Process (run)
import Oglyad.Version (EVR, parseEVR)
import System.Directory (doesDirectoryExist, doesFileExist, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- | a directory with an anda.hcl in it
data PkgDir = PkgDir
  { -- | relative to the monorepo root
    pdPath :: FilePath,
    -- | relative to the monorepo root
    pdSpec :: Maybe FilePath,
    -- | `subrepo = "extras"` from the labels block
    pdSubrepo :: Maybe Text,
    -- | file contents
    pdUpdateRhai :: Maybe Text,
    -- | `oglyad_ignore = "check, other-check"` from the labels block
    pdIgnores :: [Text]
  }
  deriving stock (Eq, Show)

data SpecInfo = SpecInfo
  { siName :: Text,
    siEVR :: EVR
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | walk anda/ and return every project directory
-- | walk anda/ and return every project directory. a directory that can't be
-- read comes back as a Left with the reason, instead of ending the walk.
-- symlinked directories aren't followed, so a link loop can't hang the walk
findPackageDirs :: FilePath -> IO [Either (FilePath, Text) PkgDir]
findPackageDirs root = go "anda"
  where
    go rel = do
      let abs' = root </> rel
      listed <- try (listDirectory abs')
      case listed of
        Left (e :: IOException) -> pure [Left (rel, T.pack (show e))]
        Right entries -> do
          hasHcl <- doesFileExist (abs' </> "anda.hcl")
          here <- if hasHcl then pure <$> readPkgDir rel entries else pure []
          subdirs <- filterM (realDir . (abs' </>)) [e | e <- sort entries, take 1 e /= "."]
          nested <- concat <$> forM subdirs (go . (rel </>))
          pure (here <> nested)

    readPkgDir rel entries = do
      r <- try (readPkgDir' rel entries)
      pure (either (\(e :: IOException) -> Left (rel, T.pack (show e))) Right r)

    readPkgDir' rel entries = do
      let abs' = root </> rel
      hcl <- readText (abs' </> "anda.hcl")
      let fromHcl = T.unpack <$> hclString "spec" hcl
          fallback = listToMaybe [e | e <- entries, takeExtension e == ".spec"]
      hasRhai <- doesFileExist (abs' </> "update.rhai")
      rhai <- if hasRhai then Just <$> readText (abs' </> "update.rhai") else pure Nothing
      pure
        PkgDir
          { pdPath = rel,
            pdSpec = (rel </>) <$> maybe fallback Just fromHcl,
            pdSubrepo = hclString "subrepo" hcl,
            pdUpdateRhai = rhai,
            pdIgnores = maybe [] (filter (not . T.null) . map T.strip . T.splitOn ",") (hclString "oglyad_ignore" hcl)
          }

-- | pull the first `key = "value"` out of anda.hcl without a real HCL parser.
-- every anda.hcl in the monorepo is simple enough for this to hold up
hclString :: Text -> Text -> Maybe Text
hclString key hcl =
  listToMaybe
    [ T.takeWhile (/= '"') (T.drop 1 (T.dropWhile (/= '"') rest))
    | l <- map T.strip (T.lines hcl),
      Just rest <- [T.stripPrefix key l],
      "=" `T.isPrefixOf` T.stripStart rest,
      "\"" `T.isInfixOf` rest
    ]

-- | a directory that isn't a symlink
realDir :: FilePath -> IO Bool
realDir path = do
  isDir <- doesDirectoryExist path
  isLink <- either (\(_ :: IOException) -> False) id <$> try (pathIsSymbolicLink path)
  pure (isDir && not isLink)

-- | read a text file as utf-8 no matter the locale, replacing invalid bytes
-- instead of failing
readText :: FilePath -> IO Text
readText path = TE.decodeUtf8Lenient <$> BS.readFile path

-- | let rpm evaluate the spec for us. hand-parsing specs means reimplementing
-- the macro engine, which is a terrible idea
querySpec :: Int -> Text -> FilePath -> IO (Either Text SpecInfo)
querySpec release dist spec = do
  let qf = "%{NAME}\t%|EPOCH?{%{EPOCH}}:{0}|:%{VERSION}-%{RELEASE}\n"
  out <-
    run "rpmspec" $
      [ "-q",
        "--srpm",
        "--define",
        "dist " <> T.unpack dist,
        -- %fedora comes from the host otherwise, so -r 43 on an F44 box would
        -- take the wrong side of every %if 0%{?fedora} >= 44
        "--define",
        "fedora " <> show release,
        -- some specs load macro files from their own dir via %{_sourcedir}
        "--define",
        "_sourcedir " <> takeDirectory spec,
        "--qf",
        qf,
        spec
      ]
  pure $ do
    o <- out
    case T.splitOn "\t" <$> take 1 (T.lines o) of
      [[name, evr]] | Just e <- parseEVR evr -> Right (SpecInfo name e)
      _ -> Left ("unexpected rpmspec output: " <> o)

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
  out <-
    run "git" $
      ["-C", root, "-c", "core.quotePath=false", "log", "--format=@%ct", "--name-only", "--no-renames", "HEAD", "--", "anda"]
  -- not a git checkout (or no git at all) just means no commit times
  pure (either (const Map.empty) (go Nothing Map.empty . T.lines) out)
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
headCommit root = either (const Nothing) (Just . T.strip) <$> run "git" ["-C", root, "rev-parse", "HEAD"]

-- | a hash of everything that can change what rpmspec says about a package:
-- the files in its directory and any subdirectories like patches/ (but not
-- nested packages, which have their own fingerprint), the dist tag, and the
-- rpm macros in effect (see 'macroFingerprint')
specFingerprint :: Text -> Text -> FilePath -> FilePath -> IO Text
specFingerprint macros dist root rel = do
  files <- packageFiles (root </> rel)
  let header = TE.encodeUtf8 (macros <> "\0" <> dist <> "\0" <> T.pack rel <> "\0")
  -- feed each file to the hash as its own chunk instead of concatenating
  -- them, which would copy every byte of the package dir once more
  ctx <-
    foldM
      ( \c (f, entry) -> do
          body <- case entry of
            -- hash where a symlink points, never what's behind it
            Link target -> pure ("<link> " <> TE.encodeUtf8 (T.pack target))
            File ->
              either (\(e :: IOException) -> "<unreadable> " <> TE.encodeUtf8 (T.pack (show e))) id
                <$> try (BS.readFile (root </> rel </> f))
          pure $! SHA256.updates c [TE.encodeUtf8 (T.pack f), "\0", body, "\0"]
      )
      (SHA256.update SHA256.init header)
      files
  pure (TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))

data Entry = File | Link FilePath

-- | every file under a package dir, relative to it and sorted, skipping
-- subdirectories that are packages of their own. symlinks are recorded with
-- their target and never followed, so a dangling link or a link loop can't
-- crash or hang the walk
packageFiles :: FilePath -> IO [(FilePath, Entry)]
packageFiles dir = go ""
  where
    go sub = do
      listed <- try (listDirectory (dir </> sub))
      let entries = either (\(_ :: IOException) -> []) sort listed
      fmap concat . forM entries $ \e -> do
        let rel = if null sub then e else sub </> e
        link <- try (getSymbolicLinkTarget (dir </> rel))
        case link of
          Right target -> pure [(rel, Link target)]
          Left (_ :: IOException) -> do
            isDir <- doesDirectoryExist (dir </> rel)
            isPkg <- doesFileExist (dir </> rel </> "anda.hcl")
            case (isDir, isPkg) of
              (True, True) -> pure []
              (True, False) -> go rel
              _ -> pure [(rel, File)]

-- | fingerprint of the whole rpm macro state, so updating rpm,
-- redhat-rpm-config or anda-srpm-macros, or editing ~/.rpmmacros, invalidates
-- cached evaluations. `rpm --showrc` dumps every macro rpm loaded, from every
-- path in its search list, which is more complete than listing files by hand
macroFingerprint :: IO Text
macroFingerprint = do
  showrc <- either ("no rpm: " <>) id <$> run "rpm" ["--showrc"]
  version <- either ("no rpmspec: " <>) id <$> run "rpmspec" ["--version"]
  pure (TE.decodeUtf8 (B16.encode (SHA256.hash (TE.encodeUtf8 (version <> "\0" <> showrc)))))
