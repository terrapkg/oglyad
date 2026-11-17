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
import System.Process.Typed (ProcessConfig, nullStream, proc, readProcess, setEnv, setStdin, setWorkingDir)
import System.Timeout (timeout)

-- | rpmspec runs shell code from specs (%(...) and %{lua:...}), so children get
-- an allowlisted environment and GITHUB_TOKEN never reaches them. stdin is
-- closed so nothing can wait on a prompt
run :: FilePath -> [String] -> IO (Either Text Text)
run cmd args = do
  env <- childEnv <$> getEnvironment
  result <- runConfined cmd (setEnv env (proc cmd args))
  pure $
    result >>= \case
      (ExitSuccess, out, _) -> Right out
      (ExitFailure _, _, err) -> Left (T.strip err)

-- | for tools that need something the allowlist leaves out, like anda needing
-- GITHUB_TOKEN
runCapture :: FilePath -> [(String, String)] -> FilePath -> [String] -> IO (Either Text (ExitCode, Text, Text))
runCapture dir extra cmd args = do
  env <- (extra <>) . filter ((`notElem` map fst extra) . fst) . childEnv <$> getEnvironment
  runConfined cmd (setWorkingDir dir (setEnv env (proc cmd args)))

timeoutMinutes :: Int
timeoutMinutes = 5

runConfined :: FilePath -> ProcessConfig stdin () () -> IO (Either Text (ExitCode, Text, Text))
runConfined cmd config = do
  r <- try (timeout (timeoutMinutes * 60 * 1_000_000) (readProcess (setStdin nullStream config)))
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack cmd <> ": " <> T.pack (show e))
    Right Nothing -> Left (T.pack cmd <> " timed out after " <> T.pack (show timeoutMinutes) <> " minutes")
    Right (Just (code, out, err)) -> Right (code, decode out, decode err)
  where
    decode = TE.decodeUtf8Lenient . LBS.toStrict

-- | GIT_TERMINAL_PROMPT=0 makes a git remote asking for credentials fail fast
-- instead of hanging
childEnv :: [(String, String)] -> [(String, String)]
childEnv parent = ("GIT_TERMINAL_PROMPT", "0") : filter (allowed . fst) parent
  where
    allowed k =
      k `elem` ["PATH", "HOME", "USER", "LOGNAME", "LANG", "TZ", "TMPDIR", "XDG_CONFIG_HOME", "XDG_CACHE_HOME", "XDG_DATA_HOME", "XDG_RUNTIME_DIR"]
        || ("LC_" `isPrefixOf` k && all (\c -> isUpper c || c == '_') k)
