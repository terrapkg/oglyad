{-# LANGUAGE ApplicativeDo #-}

-- ApplicativeDo lets configP use do-notation even though optparse's Parser
-- is only an Applicative, not a Monad
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Conc (getNumProcessors)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import Oglyad.App (HttpError (..), forConcurrentlyN, httpGetE, inRelease, note, oglyadVersion, runApp)
import Oglyad.Check (checkNames, parseCheckName, runChecks)
import Oglyad.Collect (buildSnapshot)
import Oglyad.Ignore (Applied (..), IgnoreFile (..), applyIgnores, parseIgnoreFile)
import Oglyad.Process (run)
import Oglyad.Release
import Oglyad.Report (Format (..), ReleaseInfo (..), Report (..), render)
import Oglyad.Source.Cache (loadJson, saveJson)
import Oglyad.Source.Monorepo (pruneTrees)
import Oglyad.Source.Rozbir (parseRozbirMode)
import Oglyad.Types
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout)

exitFindings, exitUsage :: ExitCode
exitFindings = ExitFailure 1
exitUsage = ExitFailure 2

data ConfigInputs = ConfigInputs
  { ciGitHubToken :: Maybe Text,
    ciCores :: Int,
    ciRunStart :: UTCTime,
    ciSpecRef :: Maybe Text,
    ciDistro :: Distro,
    ciRelease :: Release
  }

data Opts = Opts
  { optConfig :: ConfigInputs -> Config,
    optDistro :: Maybe Distro,
    optReleases :: [ReleaseArg],
    optRemote :: Text,
    optWorktree :: Bool,
    optRepo :: Maybe Text,
    optOnly :: [Text],
    optFormat :: Format,
    optMinSev :: Severity,
    optFailOn :: Maybe Severity,
    optIgnoreFile :: Maybe FilePath,
    optNoIgnores :: Bool
  }

optsP :: Parser Opts
optsP = do
  optConfig <- configP
  optDistro <- optional (option (eitherReader lookupDistro) (long "distro" <> short 'd' <> metavar "NAME" <> help ("which monorepo -m is: " <> distroNames <> ". default: whichever one the checkout looks like (it has anda/, ultramarine/ or tuatara/, and the remote's repo name tells packages and packages-el apart)")))
  optReleases <- many (option (eitherReader parseReleaseArg) (long "release" <> short 'r' <> metavar "RELEASE" <> help "release to check: 43, f43 or rawhide, el10 for Terra EL, or tumbleweed for openSUSE-based distros. repeatable. default: every release Fedora's Bodhi hasn't archived, every elN branch for terra-el, or tumbleweed"))
  optRemote <- strOption (long "remote" <> metavar "NAME" <> value "origin" <> showDefault <> help "git remote whose release branches (f43, frawhide, el10, um43, tumbleweed, ...) hold each release's specs. nothing is fetched, run git fetch yourself")
  optWorktree <- switch (long "worktree" <> help "read specs from the checkout as it is, local edits included, instead of the release branch. needs exactly one --release")
  optRepo <- optional (strOption (long "repo" <> metavar "URL" <> help ("metalink or base url of the repo to check builds against. {repo} becomes the repo's name (terra43, terra43-extras) and {release} the release's (43, rawhide). a url with neither names one release's repo, so it needs exactly one --release. default: " <> T.unpack defaultRepoUrl <> ", or https://repos.fyralabs.com/{repo} for tuatara")))
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
    distroNames = T.unpack (T.intercalate ", " (map distroName distros))
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

configP :: Parser (ConfigInputs -> Config)
configP = do
  cfgMonorepo <- strOption (long "monorepo" <> short 'm' <> metavar "DIR" <> value "." <> showDefault <> help "checkout of terrapkg/packages, terrapkg/packages-el, Ultramarine-Linux/packages or terrapkg/tuatara")
  base <- switch (long "base" <> long "fedora" <> help "also check for overlap with the base distro's repos: Fedora's release and updates (~60MB per release), AlmaLinux and EPEL for el releases, or openSUSE Tumbleweed's oss and update")
  extraBase <- many (strOption (long "base-repo" <> long "fedora-repo" <> metavar "URL" <> help "extra repo base url to check for overlap, repeatable"))
  staleDays <- option auto (long "stale-days" <> metavar "N" <> value (180 :: Int) <> showDefault <> help "builds older than this count as stale")
  cfgUpstream <- switch (long "upstream" <> short 'u' <> help "query upstream APIs for version drift")
  cacheHours <- option auto (long "cache-hours" <> metavar "N" <> value (6 :: Int) <> showDefault <> help "reuse upstream answers younger than this, 0 to always refetch")
  jobs <- optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "parallel jobs (default: number of cores)"))
  cfgRozbir <- option (eitherReader parseRozbirMode) (long "rozbir" <> metavar "MODE" <> value RozbirFirst <> help "how specs get evaluated: first asks rozbir and only runs rpmspec for specs it can't finish (the default), shadow runs both and reports where they differ, off always runs rpmspec")
  pure $ \ConfigInputs {ciDistro = distro, ciRelease = rel, ..} ->
    Config
      { cfgGitHubToken = ciGitHubToken,
        cfgRunStart = ciRunStart,
        cfgSpecRef = ciSpecRef,
        cfgRoot = distroRoot distro,
        cfgRepoName = repoName distro rel,
        cfgRepoUrl = repoUrlFor (distroRepoUrl distro) rel,
        cfgSubrepos = releaseSubrepos distro rel,
        cfgBaseRepos = (if base then baseReposFor rel else []) <> extraBase,
        cfgBaseName = baseRepoOwners (relBase rel),
        cfgMacros = releaseMacros distro rel,
        cfgCacheTag = releaseCacheTag distro rel,
        cfgBranch = releaseBranch distro rel,
        cfgRolling = isRolling rel,
        cfgStaleAfter = fromIntegral (staleDays * 86400),
        cfgCacheFor = fromIntegral (cacheHours * 3600),
        cfgConcurrency = fromMaybe ciCores jobs,
        ..
      }

-- | EL releases come from the remote's elN branches, not Bodhi, which lists
-- EPEL releases Terra doesn't build
resolveReleases :: FilePath -> Text -> Distro -> (Release -> Config) -> [ReleaseArg] -> IO [Release]
resolveReleases mono remote distro mk args
  | null args = case distroBases distro of
      OpenSUSE : _ -> pure [tumbleweed]
      EL : _ ->
        elReleasesFrom <$> remoteBranches mono remote >>= \case
          [] -> runError ("the " <> remote <> " remote has no el10-style branches, so there's no EL release to check. git fetch, or pass --release")
          rels -> pure rels
      _ -> bodhi >>= either (\e -> runError ("can't ask Bodhi which releases are maintained: " <> e <> ". pass --release to pick them")) pure
  | otherwise = nub <$> mapM resolve args
  where
    resolve arg
      | releaseArgBase arg `notElem` distroBases distro =
          runError (distroName distro <> " is built on " <> T.intercalate " and " (map baseName (distroBases distro)) <> ", so it has no " <> baseName (releaseArgBase arg) <> " releases")
    resolve arg = case arg of
      TumbleweedArg -> pure tumbleweed
      ElArg n -> pure (el n)
      NumberedArg n -> pure (Release Fedora (T.pack (show n)) n)
      RawhideArg ->
        bodhi >>= \case
          Right rels | r : _ <- filter isRolling rels -> pure r
          Right _ -> runError "Bodhi didn't list a rawhide release"
          Left e -> runError ("can't ask Bodhi for rawhide's version: " <> e <> ". pass numbered releases instead")
    bodhi = askBodhiPatiently (mk anyRelease)
    runError msg = hPutStrLn stderr ("oglyad: " <> T.unpack msg) >> exitWith exitUsage

remoteBranches :: FilePath -> Text -> IO [Text]
remoteBranches mono remote =
  either (const []) T.lines
    <$> run "git" ["-C", mono, "for-each-ref", "--format=%(refname:lstrip=3)", "refs/remotes/" <> T.unpack remote <> "/"]

bodhiAttempts :: Int
bodhiAttempts = 4

bodhiCacheFile :: FilePath
bodhiCacheFile = "bodhi-releases.json"

askBodhiPatiently :: Config -> IO (Either Text [Release])
askBodhiPatiently cfg = go 1
  where
    go attempt =
      askBodhi cfg >>= \case
        Right rels -> do
          now <- getCurrentTime
          saveJson bodhiCacheFile (now, [(relName r, relVersion r) | r <- rels])
          pure (Right rels)
        Left e
          | attempt < bodhiAttempts -> do
              let waitSecs = 5 * 2 ^ (attempt - 1)
              warn ("Bodhi didn't answer (" <> e <> "), trying again in " <> T.pack (show waitSecs) <> "s")
              threadDelay (waitSecs * 1_000_000)
              go (attempt + 1)
          | otherwise -> fallBackToCache e
    fallBackToCache e =
      loadJson bodhiCacheFile >>= \case
        Just (fetched :: UTCTime, rels) -> do
          warn ("Bodhi didn't answer (" <> e <> "), using its release list from " <> T.pack (show fetched))
          pure (Right [Release Fedora name v | (name, v) <- rels])
        Nothing -> pure (Left e)
    warn msg = hPutStrLn stderr ("oglyad: " <> T.unpack msg)

askBodhi :: Config -> IO (Either Text [Release])
askBodhi cfg = do
  body <- runApp cfg (httpGetE bodhiReleasesUrl)
  pure $ case body of
    Left (HttpFailed e) -> Left e
    Left (RateLimited e) -> Left e
    Right b -> parseBodhiReleases b

-- | oglyad only sends this to api.github.com. the one child process that gets
-- it is anda, since its GitHub helpers need it
githubToken :: IO (Maybe Text)
githubToken =
  lookupEnv "GITHUB_TOKEN" >>= \case
    Just t | not (null t) -> pure (Just (T.pack t))
    _ -> either (const Nothing) (nonEmpty . T.strip) <$> run "gh" ["auth", "token"]
  where
    nonEmpty t = if T.null t then Nothing else Just t

parseOpts :: IO Opts
parseOpts = do
  args <- getArgs
  prog <- getProgName
  let pinfo =
        info
          (optsP <**> helper <**> simpleVersioner ("oglyad " <> T.unpack oglyadVersion))
          (fullDesc <> progDesc "repository metadata and health checker for Terra and other anda monorepos")
  case execParserPure defaultPrefs pinfo args of
    Failure f -> case renderFailure f prog of
      -- help2man and friends read --help and --version from stdout
      (msg, ExitSuccess) -> putStrLn msg >> exitWith ExitSuccess
      (msg, _) -> hPutStrLn stderr msg >> exitWith exitUsage
    other -> handleParseResult other

pickDistro :: FilePath -> Text -> Maybe Distro -> IO Distro
pickDistro mono remote asked = do
  found <- filterM (doesDirectoryExist . (mono </>) . distroMarker) distros
  case asked of
    Just d
      | d `elem` found -> pure d
      | otherwise -> usageError (mono <> " doesn't look like a " <> T.unpack (distroName d) <> " checkout (no " <> distroMarker d <> "/ directory). point -m at one")
    Nothing -> case found of
      [d] -> pure d
      [] -> usageError (mono <> " doesn't look like an anda monorepo (no " <> markers <> " directory). point -m at a checkout of terrapkg/packages or another, see --distro")
      _ -> do
        upstream <- either (const Nothing) remoteRepoName <$> run "git" ["-C", mono, "remote", "get-url", T.unpack remote]
        case [d | d <- found, Just (distroUpstream d) == upstream] of
          [d] -> pure d
          _ -> usageError (mono <> " looks like more than one distro (" <> T.unpack (T.intercalate ", " (map distroName found)) <> ") and its " <> T.unpack remote <> " remote doesn't say which, so pick one with --distro")
  where
    markers = T.unpack (T.intercalate "/, " (nubOrd (map (T.pack . distroMarker) distros))) <> "/"

usageError :: String -> IO a
usageError msg = hPutStrLn stderr ("oglyad: " <> msg) >> exitWith exitUsage

-- | cron and CI often run under LANG=C, where the non-ascii text in specs,
-- anda.hcl and rpmspec errors would crash text IO
useUtf8Everywhere :: IO ()
useUtf8Everywhere = do
  mapM_ ($ utf8) [setLocaleEncoding, setFileSystemEncoding, setForeignEncoding]
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]

anyRelease :: Release
anyRelease = Release Fedora "rawhide" 0

namesOneRepo :: Text -> Bool
namesOneRepo url = not (any (`T.isInfixOf` url) ["{repo}", "{release}"])

main :: IO ()
main = do
  useUtf8Everywhere
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
  when (maybe False namesOneRepo optRepo && length optReleases /= 1) $
    usageError "--repo without {repo} or {release} in it names one repo, so it needs exactly one --release"
  token <- githubToken
  cores <- getNumProcessors
  start <- getCurrentTime
  let configFor specRef distro rel = optConfig (ConfigInputs token cores start specRef distro rel)
      mono = cfgMonorepo (configFor Nothing terra anyRelease)
  distro <- pickDistro mono optRemote optDistro
  let baseCfg = configFor Nothing distro anyRelease
  let specRef rel = if optWorktree then Nothing else Just (optRemote <> "/" <> releaseBranch distro rel)
      mkCfg rel =
        let c = configFor (specRef rel) distro rel
         in maybe c (\url -> c {cfgRepoUrl = repoUrlFor url rel}) optRepo
  releases <- resolveReleases mono optRemote distro mkCfg optReleases

  when (cfgUpstream baseCfg && isNothing token) $
    hPutStrLn stderr "oglyad: no GITHUB_TOKEN (or `gh auth token`), GitHub lookups will hit the 60/hour limit fast"

  let ran = filter (`notElem` distroSkippedChecks distro) (if null optOnly then checkNames else optOnly)
      logPrefix rel = if length releases > 1 then T.unpack (releaseLabel rel) <> ": " else ""
  perRelease <- runApp baseCfg . forConcurrentlyN (length releases) releases $ \rel -> do
    let cfg = mkCfg rel
    (snap, collectFindings) <- inRelease cfg (logPrefix rel) $ do
      note ("checking " <> T.unpack (releaseLabel rel) <> " from " <> maybe "the working tree" T.unpack (cfgSpecRef cfg))
      buildSnapshot
    let findings = collectFindings <> runChecks ran snap
        applied
          | optNoIgnores = Applied findings [] [] []
          | otherwise = applyIgnores checkNames ran ignores (snapIgnoreLabels snap) findings
        relInfo = ReleaseInfo (relName rel) (releaseLabel rel) (cfgSpecRef cfg) (snapHead snap) (repoUrl cfg Nothing)
    pure (rel, relInfo, applied)
  unless optWorktree $
    pruneTrees [c | (_, ReleaseInfo {riCommit = Just c}, _) <- perRelease]

  now <- getCurrentTime
  let merged f = mergeFindings distro [(rel, f a) | (rel, _, a) <- perRelease]
      unusedInEveryRelease = [f | f <- merged apUnused, length (fReleases f) == length releases]
      report =
        Report
          { rVersion = oglyadVersion,
            rGenerated = now,
            rReleases = [ri | (_, ri, _) <- perRelease],
            rDistro = distroName distro,
            rUpstream = any (\(rel, _, _) -> cfgUpstream (mkCfg rel)) perRelease,
            rFindings = merged apKept <> merged apProblems <> unusedInEveryRelease,
            rIgnored = merged apIgnored
          }
  LBS.putStr (render optFormat optMinSev report)
  when (maybe False (\level -> any ((>= level) . fSeverity) (rFindings report)) optFailOn) $
    exitWith exitFindings
