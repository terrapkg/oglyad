{-# LANGUAGE ApplicativeDo #-}

-- ApplicativeDo lets configP use do-notation even though optparse's Parser
-- is only an Applicative, not a Monad
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Conc (getNumProcessors)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import Oglyad.App (oglyadVersion, runApp)
import Oglyad.Check (checkNames, parseCheckName, runChecks)
import Oglyad.Collect (buildSnapshot)
import Oglyad.Ignore (Applied (..), IgnoreFile (..), applyIgnores, parseIgnoreFile)
import Oglyad.Process (run)
import Oglyad.Report (Format (..), Report (..), render)
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
  { optConfig :: Maybe Text -> Int -> Config,
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

configP :: Parser (Maybe Text -> Int -> Config)
configP = do
  cfgMonorepo <- strOption (long "monorepo" <> short 'm' <> metavar "DIR" <> value "." <> showDefault <> help "checkout of terrapkg/packages")
  release <- option auto (long "release" <> short 'r' <> metavar "N" <> value (44 :: Int) <> showDefault <> help "Fedora/Terra release to check")
  terraRepo <- optional (strOption (long "repo" <> metavar "URL" <> help "Terra repo base url (default: https://repos.fyralabs.com/terraN)"))
  fedora <- switch (long "fedora" <> help "also check for overlap with Fedora's release and updates repos (downloads ~60MB)")
  extraFedora <- many (strOption (long "fedora-repo" <> metavar "URL" <> help "extra repo base url to check for overlap, repeatable"))
  staleDays <- option auto (long "stale-days" <> metavar "N" <> value (180 :: Int) <> showDefault <> help "builds older than this count as stale")
  cfgUpstream <- switch (long "upstream" <> short 'u' <> help "query upstream APIs for version drift")
  cacheHours <- option auto (long "cache-hours" <> metavar "N" <> value (6 :: Int) <> showDefault <> help "reuse upstream answers younger than this, 0 to always refetch")
  jobs <- optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "parallel jobs (default: number of cores)"))
  pure $ \cfgGitHubToken cores ->
    let r = T.pack (show release)
        fedoraRepos =
          [ "https://dl.fedoraproject.org/pub/fedora/linux/releases/" <> r <> "/Everything/x86_64/os",
            "https://dl.fedoraproject.org/pub/fedora/linux/updates/" <> r <> "/Everything/x86_64"
          ]
     in Config
          { cfgTerraRepo = maybe ("https://repos.fyralabs.com/terra" <> r) id terraRepo,
            cfgFedoraRepos = (if fedora then fedoraRepos else []) <> extraFedora,
            cfgRelease = release,
            cfgDist = ".fc" <> r,
            cfgStaleAfter = fromIntegral (staleDays * 86400),
            cfgCacheFor = fromIntegral (cacheHours * 3600),
            cfgConcurrency = maybe cores id jobs,
            ..
          }

-- | GITHUB_TOKEN, or whatever the gh cli is logged in with. the token is only
-- ever sent to api.github.com, and never passed to child processes
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
    Failure f -> do
      let (msg, code) = renderFailure f prog
      hPutStrLn stderr msg
      exitWith (if code == ExitSuccess then ExitSuccess else exitUsage)
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
  token <- githubToken
  cores <- getNumProcessors
  let cfg = optConfig token cores
  hasAnda <- doesDirectoryExist (cfgMonorepo cfg </> "anda")
  unless hasAnda $
    usageError (cfgMonorepo cfg <> " doesn't look like a terrapkg/packages checkout (no anda/ directory). point -m at one")

  (snap, collectFindings) <- runApp cfg buildSnapshot
  now <- getCurrentTime
  let ran = if null optOnly then checkNames else optOnly
      findings = collectFindings <> runChecks optOnly snap
      Applied {..}
        | optNoIgnores = Applied findings [] []
        | otherwise = applyIgnores checkNames ran ignores (snapIgnoreLabels snap) findings
      report =
        Report
          { rVersion = oglyadVersion,
            rGenerated = now,
            rMonorepoHead = snapHead snap,
            rTerraRepo = cfgTerraRepo cfg,
            rUpstream = cfgUpstream cfg,
            rFindings = apKept <> apProblems,
            rIgnored = apIgnored
          }
  LBS.putStr (render optFormat optMinSev report)
  when (maybe False (\level -> any ((>= level) . fSeverity) (rFindings report)) optFailOn) $
    exitWith exitFindings
