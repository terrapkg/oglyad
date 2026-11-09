{-# LANGUAGE ApplicativeDo #-}

-- ApplicativeDo lets configP use do-notation even though optparse's Parser
-- is only an Applicative, not a Monad
module Main (main) where

import Control.Exception (SomeException, try)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Conc (getNumProcessors)
import Oglyad.App (runApp)
import Oglyad.Check (allChecks, runChecks)
import Oglyad.Collect (buildSnapshot)
import Oglyad.Report (renderJson, renderText)
import Oglyad.Types
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.Process.Typed (proc, readProcessStdout)

data Opts = Opts
  { optConfig :: Maybe Text -> Int -> Config
  , optOnly :: [Text]
  , optJson :: Bool
  , optMinSev :: Severity
  }

optsP :: Parser Opts
optsP = Opts <$> configP <*> onlyP <*> jsonP <*> minSevP
  where
    onlyP = many (strOption (long "only" <> metavar "CHECK" <> help ("run only this check, repeatable. one of: " <> checkNames)))
    jsonP = switch (long "json" <> help "emit findings as JSON")
    minSevP =
      flag' Info (long "verbose" <> short 'v' <> help "show info findings too")
        <|> pure Warn
    checkNames = T.unpack (T.intercalate ", " (map fst allChecks))

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
          [ "https://dl.fedoraproject.org/pub/fedora/linux/releases/" <> r <> "/Everything/x86_64/os"
          , "https://dl.fedoraproject.org/pub/fedora/linux/updates/" <> r <> "/Everything/x86_64"
          ]
     in Config
          { cfgTerraRepo = maybe ("https://repos.fyralabs.com/terra" <> r) id terraRepo
          , cfgFedoraRepos = (if fedora then fedoraRepos else []) <> extraFedora
          , cfgDist = ".fc" <> r
          , cfgStaleAfter = fromIntegral (staleDays * 86400)
          , cfgCacheFor = fromIntegral (cacheHours * 3600)
          , cfgConcurrency = maybe cores id jobs
          , ..
          }

-- | GITHUB_TOKEN, or whatever the gh cli is logged in with. the token is only
-- ever sent to api.github.com
githubToken :: IO (Maybe Text)
githubToken =
  lookupEnv "GITHUB_TOKEN" >>= \case
    Just t | not (null t) -> pure (Just (T.pack t))
    _ -> do
      r <- try (readProcessStdout (proc "gh" ["auth", "token"]))
      pure $ case r :: Either SomeException (ExitCode, LBS8.ByteString) of
        Right (ExitSuccess, out) | t <- T.strip (T.pack (LBS8.unpack out)), not (T.null t) -> Just t
        _ -> Nothing

main :: IO ()
main = do
  Opts {..} <- execParser (info (optsP <**> helper) (fullDesc <> progDesc "repository metadata and health checker for Terra"))
  token <- githubToken
  cores <- getNumProcessors
  let cfg = optConfig token cores
  (snap, collectFindings) <- runApp cfg buildSnapshot
  let findings = collectFindings <> runChecks optOnly snap
  if optJson
    then LBS8.putStrLn (renderJson findings)
    else TIO.putStr (renderText optMinSev findings)
  if any ((== Error) . fSeverity) findings then exitFailure else pure ()
