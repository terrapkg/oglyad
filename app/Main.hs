{-# LANGUAGE ApplicativeDo #-}

-- ApplicativeDo lets configP use do-notation even though optparse's Parser
-- is only an Applicative, not a Monad
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Conc (getNumProcessors)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import Oglyad.App (HttpError (..), httpGetE, oglyadVersion, runApp)
import Oglyad.Check (checkNames, parseCheckName, runChecks)
import Oglyad.Collect (buildSnapshot)
import Oglyad.Ignore (Applied (..), IgnoreFile (..), applyIgnores, parseIgnoreFile)
import Oglyad.Process (run)
import Oglyad.Release
import Oglyad.Report (Format (..), ReleaseInfo (..), Report (..), render)
import Oglyad.Source.Monorepo (pruneTrees)
import Oglyad.Types
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout)

-- | exit codes, so cron jobs and CI can tell "found problems" apart from
-- "couldn't run"
exitFindings, exitUsage :: ExitCode
exitFindings = ExitFailure 1
exitUsage = ExitFailure 2

data Opts = Opts
  { -- | token, cores, run start, spec ref, release
    optConfig :: Maybe Text -> Int -> UTCTime -> Maybe Text -> Release -> Config,
    optReleases :: [ReleaseArg],
    optRemote :: Text,
    optWorktree :: Bool,
    optRepo :: Maybe Text,
    optOnly :: [Text],
    optFormat :: Format,
    optMinSev :: Severity,
    -- | Nothing means never fail on findings
    optFailOn :: Maybe Severity,
    optIgnoreFile :: Maybe FilePath,
    optNoIgnores :: Bool
  }

optsP :: Parser Opts
optsP = do
  optConfig <- configP
  optReleases <- many (option (eitherReader parseReleaseArg) (long "release" <> short 'r' <> metavar "RELEASE" <> help "release to check: 43, f43 or rawhide. repeatable. default: every release Fedora's Bodhi hasn't archived"))
  optRemote <- strOption (long "remote" <> metavar "NAME" <> value "origin" <> showDefault <> help "git remote whose release branches (f43, frawhide, ...) hold each release's specs. nothing is fetched, run git fetch yourself")
  optWorktree <- switch (long "worktree" <> help "read specs from the checkout as it is, local edits included, instead of the release branch. needs exactly one --release")
  optRepo <- optional (strOption (long "repo" <> metavar "URL" <> help "Terra repo base url (default: https://repos.fyralabs.com/terraN). needs exactly one --release"))
  optOnly <- many (option (eitherReader parseCheckName) (long "only" <> metavar "CHECK" <> help ("run only this check, repeatable. one of: " <> names)))
  optFormat <-
    option (eitherReader format) (long "format" <> short 'f' <> metavar "FORMAT" <> help "text (default), json, or markdown")
      <|> flag' Json (long "json" <> help "same as --format json")
      <|> pure Text
  optMinSev <-
    flag' Info (long "verbose" <> short 'v' <> help "show info findings too")
      <|> pure Warn
  optFailOn <- option (eitherReader failOn) (long "fail-on" <> metavar "LEVEL" <> value (Just Error) <> help "exit 1 if any finding is at least this severe: error (default), warn, info, or never")
  optIgnoreFile <- optional (strOption (long "ignore-file" <> metavar "FILE" <> help "json file of known findings to ignore, see README"))
  optNoIgnores <- switch (long "no-ignores" <> help "report everything, ignoring the ignore file and anda.hcl oglyad_ignore labels")
  pure Opts {..}
  where
    names = T.unpack (T.intercalate ", " checkNames)
    format = \case
      "text" -> Right Text
      "json" -> Right Json
      "markdown" -> Right Markdown
      other -> Left ("unknown format " <> show other <> ", expected text, json or markdown")
    failOn = \case
      "error" -> Right (Just Error)
      "warn" -> Right (Just Warn)
      "info" -> Right (Just Info)
      "never" -> Right Nothing
      other -> Left ("unknown level " <> show other <> ", expected error, warn, info or never")

configP :: Parser (Maybe Text -> Int -> UTCTime -> Maybe Text -> Release -> Config)
configP = do
  cfgMonorepo <- strOption (long "monorepo" <> short 'm' <> metavar "DIR" <> value "." <> showDefault <> help "checkout of terrapkg/packages")
  fedora <- switch (long "fedora" <> help "also check for overlap with Fedora's release and updates repos (downloads ~60MB per release)")
  extraFedora <- many (strOption (long "fedora-repo" <> metavar "URL" <> help "extra repo base url to check for overlap, repeatable"))
  staleDays <- option auto (long "stale-days" <> metavar "N" <> value (180 :: Int) <> showDefault <> help "builds older than this count as stale")
  cfgUpstream <- switch (long "upstream" <> short 'u' <> help "query upstream APIs for version drift")
  cacheHours <- option auto (long "cache-hours" <> metavar "N" <> value (6 :: Int) <> showDefault <> help "reuse upstream answers younger than this, 0 to always refetch")
  jobs <- optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "parallel jobs (default: number of cores)"))
  pure $ \cfgGitHubToken cores cfgRunStart cfgSpecRef rel ->
    Config
      { cfgTerraRepo = terraRepoFor rel,
        cfgFedoraRepos = (if fedora then fedoraReposFor rel else []) <> extraFedora,
        cfgRelease = relVersion rel,
        cfgDist = releaseDist rel,
        cfgBranch = releaseBranch rel,
        cfgStaleAfter = fromIntegral (staleDays * 86400),
        cfgCacheFor = fromIntegral (cacheHours * 3600),
        cfgConcurrency = maybe cores id jobs,
        ..
      }

-- | turn --release arguments into releases, asking Bodhi for the
-- maintained ones when none were given, and for rawhide's number when
-- rawhide was asked for
resolveReleases :: (Release -> Config) -> [ReleaseArg] -> IO [Release]
resolveReleases mk args
  | null args = bodhi >>= either (\e -> runError ("can't ask Bodhi which releases are maintained: " <> e <> ". pass --release to pick them")) pure
  | otherwise = nub <$> mapM resolve args
  where
    resolve = \case
      NumberedArg n -> pure (Release (T.pack (show n)) n)
      RawhideArg ->
        bodhi >>= \case
          Right rels | r : _ <- filter isRawhide rels -> pure r
          Right _ -> runError "Bodhi didn't list a rawhide release"
          Left e -> runError ("can't ask Bodhi for rawhide's version: " <> e <> ". pass numbered releases instead")
    -- runApp wants a config. none of it matters for one http request
    bodhi = runApp (mk (Release "rawhide" 0)) $ do
      body <- httpGetE bodhiReleasesUrl
      pure $ case body of
        Left (HttpFailed e) -> Left e
        Left (RateLimited e) -> Left e
        Right b -> parseBodhiReleases b
    runError msg = hPutStrLn stderr ("oglyad: " <> T.unpack msg) >> exitWith exitUsage

-- | GITHUB_TOKEN, or whatever the gh cli is logged in with. oglyad itself
-- only sends it to api.github.com. the one child process that gets it is
-- anda (see Oglyad.Source.Anda), since its GitHub helpers need it
githubToken :: IO (Maybe Text)
githubToken =
  lookupEnv "GITHUB_TOKEN" >>= \case
    Just t | not (null t) -> pure (Just (T.pack t))
    _ -> either (const Nothing) (nonEmpty . T.strip) <$> run "gh" ["auth", "token"]
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- | like execParser, but bad arguments exit with 'exitUsage' instead of 1
parseOpts :: IO Opts
parseOpts = do
  args <- getArgs
  prog <- getProgName
  let pinfo =
        info
          (optsP <**> helper <**> simpleVersioner ("oglyad " <> T.unpack oglyadVersion))
          (fullDesc <> progDesc "repository metadata and health checker for Terra")
  case execParserPure defaultPrefs pinfo args of
    Failure f -> case renderFailure f prog of
      -- --help and --version are output, not errors, so they go to stdout
      -- (help2man and friends read them from there)
      (msg, ExitSuccess) -> putStrLn msg >> exitWith ExitSuccess
      (msg, _) -> hPutStrLn stderr msg >> exitWith exitUsage
    other -> handleParseResult other

usageError :: String -> IO a
usageError msg = hPutStrLn stderr ("oglyad: " <> msg) >> exitWith exitUsage

main :: IO ()
main = do
  -- cron and CI often run under LANG=C. specs, anda.hcl and rpmspec errors
  -- contain non-ascii text, which would crash text IO in a C locale
  mapM_ ($ utf8) [setLocaleEncoding, setFileSystemEncoding, setForeignEncoding]
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]

  Opts {..} <- parseOpts
  ignores <- case optIgnoreFile of
    Nothing -> pure (IgnoreFile [])
    Just path -> do
      raw <- try (BS.readFile path)
      case raw of
        Left (e :: SomeException) -> usageError ("can't read ignore file " <> path <> ": " <> show e)
        Right bytes -> either (\e -> usageError (path <> ": " <> e)) pure (parseIgnoreFile checkNames bytes)
  when (optWorktree && length optReleases /= 1) $
    usageError "--worktree checks the working tree against one release, so it needs exactly one --release"
  when (isJust optRepo && length optReleases /= 1) $
    usageError "--repo names one Terra repo, so it needs exactly one --release"
  token <- githubToken
  cores <- getNumProcessors
  start <- getCurrentTime
  let specRef rel = if optWorktree then Nothing else Just (optRemote <> "/" <> releaseBranch rel)
      mkCfg rel =
        let c = optConfig token cores start (specRef rel) rel
         in maybe c (\url -> c {cfgTerraRepo = url}) optRepo
  hasAnda <- doesDirectoryExist (cfgMonorepo (mkCfg (Release "rawhide" 0)) </> "anda")
  unless hasAnda $
    usageError (cfgMonorepo (mkCfg (Release "rawhide" 0)) <> " doesn't look like a terrapkg/packages checkout (no anda/ directory). point -m at one")
  releases <- resolveReleases mkCfg optReleases

  let ran = if null optOnly then checkNames else optOnly
  perRelease <- forM releases $ \rel -> do
    let cfg = mkCfg rel
    hPutStrLn stderr ("oglyad: checking " <> T.unpack (releaseLabel rel) <> " from " <> maybe "the working tree" T.unpack (cfgSpecRef cfg))
    (snap, collectFindings) <- runApp cfg buildSnapshot
    let findings = collectFindings <> runChecks optOnly snap
        applied
          | optNoIgnores = Applied findings [] [] []
          | otherwise = applyIgnores checkNames ran ignores (snapIgnoreLabels snap) findings
        relInfo = ReleaseInfo (relName rel) (releaseLabel rel) (cfgSpecRef cfg) (snapHead snap) (cfgTerraRepo cfg)
    pure (rel, relInfo, applied)
  -- extracted trees for commits nothing checks anymore are just disk space
  unless optWorktree $
    pruneTrees [c | (_, ReleaseInfo {riCommit = Just c}, _) <- perRelease]

  now <- getCurrentTime
  let merged f = mergeFindings [(rel, f a) | (rel, _, a) <- perRelease]
      -- an ignore entry is only unused if no release needed it
      unused = [f | f <- merged apUnused, length (fReleases f) == length releases]
      report =
        Report
          { rVersion = oglyadVersion,
            rGenerated = now,
            rReleases = [ri | (_, ri, _) <- perRelease],
            rUpstream = any (\(rel, _, _) -> cfgUpstream (mkCfg rel)) perRelease,
            rFindings = merged apKept <> merged apProblems <> unused,
            rIgnored = merged apIgnored
          }
  LBS.putStr (render optFormat optMinSev report)
  when (maybe False (\level -> any ((>= level) . fSeverity) (rFindings report)) optFailOn) $
    exitWith exitFindings
