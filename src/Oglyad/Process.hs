-- | running external tools (rpmspec, git) without ever throwing, hanging, or
-- leaking secrets
module Oglyad.Process
  ( run,
    runCapture,
    childEnv,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, readProcess, setEnv, setStdin, setWorkingDir)
import System.Timeout (timeout)

-- | run a command and return its stdout. a missing executable, a non-zero
-- exit, or a timeout becomes a Left, so callers can report it as a finding
-- instead of crashing the whole run.
--
-- rpmspec runs shell code from specs (%(...) and %{lua:...}), so children get
-- an allowlisted environment: GITHUB_TOKEN and anything else secret in ours
-- never reaches them. stdin is closed so nothing can wait on a prompt
run :: FilePath -> [String] -> IO (Either Text Text)
run cmd args = do
  env <- childEnv <$> getEnvironment
  r <- try (timeout (5 * 60 * 1_000_000) (readProcess (setEnv env (setStdin nullStream (proc cmd args)))))
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack cmd <> ": " <> T.pack (show e))
    Right Nothing -> Left (T.pack cmd <> " timed out after 5 minutes")
    Right (Just (ExitSuccess, out, _)) -> Right (decode out)
    Right (Just (ExitFailure _, _, err)) -> Left (T.strip (decode err))
  where
    decode = TE.decodeUtf8Lenient . LBS.toStrict

-- | like 'run', but in a working directory, with extra variables on top of
-- the allowlist, and handing back the exit code, stdout and stderr even when
-- the command succeeded. only for tools that need something the allowlist
-- leaves out, like anda needing GITHUB_TOKEN
runCapture :: FilePath -> [(String, String)] -> FilePath -> [String] -> IO (Either Text (ExitCode, Text, Text))
runCapture dir extra cmd args = do
  env <- (extra <>) . filter ((`notElem` map fst extra) . fst) . childEnv <$> getEnvironment
  r <- try (timeout (5 * 60 * 1_000_000) (readProcess (setWorkingDir dir (setEnv env (setStdin nullStream (proc cmd args))))))
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack cmd <> ": " <> T.pack (show e))
    Right Nothing -> Left (T.pack cmd <> " timed out after 5 minutes")
    Right (Just (code, out, err)) -> Right (code, decode out, decode err)
  where
    decode = TE.decodeUtf8Lenient . LBS.toStrict

-- | the environment children get: only what rpm and git need to find their
-- config and behave, plus GIT_TERMINAL_PROMPT=0 so a git remote asking for
-- credentials fails fast instead of hanging
childEnv :: [(String, String)] -> [(String, String)]
childEnv parent = ("GIT_TERMINAL_PROMPT", "0") : filter (allowed . fst) parent
  where
    allowed k =
      k `elem` ["PATH", "HOME", "USER", "LOGNAME", "LANG", "TZ", "TMPDIR", "XDG_CONFIG_HOME", "XDG_CACHE_HOME", "XDG_DATA_HOME", "XDG_RUNTIME_DIR"]
        || ("LC_" `isPrefixOf` k && all (\c -> isUpper c || c == '_') k)
