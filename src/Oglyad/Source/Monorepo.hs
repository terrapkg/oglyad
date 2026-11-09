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

import Control.Exception (SomeException, try)
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
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Oglyad.Process (run)
import Oglyad.Version (EVR, parseEVR)
import System.Directory (XdgDirectory (XdgConfig), doesDirectoryExist, doesFileExist, getHomeDirectory, getXdgDirectory, listDirectory)
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
    pdUpdateRhai :: Maybe Text
  }
  deriving stock (Eq, Show)

data SpecInfo = SpecInfo
  { siName :: Text,
    siEVR :: EVR
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
          { pdPath = rel,
            pdSpec = (rel </>) <$> maybe fallback Just fromHcl,
            pdSubrepo = hclString "subrepo" hcl,
            pdUpdateRhai = rhai
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

-- | let rpm evaluate the spec for us. hand-parsing specs means reimplementing
-- the macro engine, which is a terrible idea
querySpec :: Text -> FilePath -> IO (Either Text SpecInfo)
querySpec dist spec = do
  let qf = "%{NAME}\t%|EPOCH?{%{EPOCH}}:{0}|:%{VERSION}-%{RELEASE}\n"
  out <-
    run "rpmspec" $
      [ "-q",
        "--srpm",
        "--define",
        "dist " <> T.unpack dist,
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
      ( \c f -> do
          body <- BS.readFile (root </> rel </> f)
          pure $! SHA256.updates c [TE.encodeUtf8 (T.pack f), "\0", body, "\0"]
      )
      (SHA256.update SHA256.init header)
      files
  pure (TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))

-- | every file under a package dir, relative to it and sorted, skipping
-- subdirectories that are packages of their own
packageFiles :: FilePath -> IO [FilePath]
packageFiles dir = go ""
  where
    go sub = do
      entries <- sort <$> listDirectory (dir </> sub)
      fmap concat . forM entries $ \e -> do
        let rel = if null sub then e else sub </> e
        isDir <- doesDirectoryExist (dir </> rel)
        isPkg <- doesFileExist (dir </> rel </> "anda.hcl")
        case (isDir, isPkg) of
          (True, True) -> pure []
          (True, False) -> go rel
          _ -> pure [rel]

-- | fingerprint of every rpm macro file that can affect spec evaluation:
-- the system ones (so updating anda-srpm-macros or rpm itself invalidates
-- cached evaluations) and the user's own ~/.rpmmacros and
-- ~/.config/rpm/macros. it hashes contents, which for a few hundred small
-- files is still only a millisecond or two
macroFingerprint :: IO Text
macroFingerprint = do
  home <- getHomeDirectory
  xdg <- getXdgDirectory XdgConfig "rpm"
  let dirs = ["/usr/lib/rpm/macros.d", "/etc/rpm"]
      single = ["/usr/lib/rpm/macros", "/usr/lib/rpm/rpmrc", home </> ".rpmmacros", xdg </> "macros"]
  listed <- fmap concat . forM dirs $ \d -> do
    r <- try (listDirectory d) :: IO (Either SomeException [FilePath])
    pure (map (d </>) (either (const []) sort r))
  -- the rpmspec binary itself, by version rather than by bytes
  version <- either (const "no rpmspec") id <$> run "rpmspec" ["--version"]
  ctx <-
    foldM
      ( \c f -> do
          r <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
          -- a missing file still counts, so creating ~/.rpmmacros changes the hash
          pure $! SHA256.updates c [TE.encodeUtf8 (T.pack f), "\0", either (const "<missing>") id r, "\0"]
      )
      (SHA256.update SHA256.init (TE.encodeUtf8 version))
      (single <> listed)
  pure (TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))
