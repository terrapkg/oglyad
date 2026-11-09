-- | running external tools (rpmspec, git) without ever throwing
module Oglyad.Process
  ( run,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess)

-- | run a command and return its stdout. a missing executable or a non-zero
-- exit becomes a Left carrying stderr, so callers can report it as a finding
-- instead of crashing the whole run
run :: FilePath -> [String] -> IO (Either Text Text)
run cmd args = do
  r <- try (readProcess (proc cmd args))
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack cmd <> ": " <> T.pack (show e))
    Right (ExitSuccess, out, _) -> Right (decode out)
    Right (ExitFailure _, _, err) -> Left (T.strip (decode err))
  where
    decode = TE.decodeUtf8Lenient . LBS.toStrict
