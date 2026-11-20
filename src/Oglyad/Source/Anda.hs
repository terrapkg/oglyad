-- | anda rewrites specs in place, so it runs on a throwaway copy of the tree.
-- whatever Version the copy's spec says afterwards is what `anda update`
-- would have set
module Oglyad.Source.Anda
  ( withScratchCopy,
    projectNames,
    reportSupported,
    updateProject,
    parseReport,
    ProjectRun (..),
    RunStatus (..),
    RunError (..),
    describeError,
    stripAnsi,
  )
where

import Control.Exception (SomeException, finally, try)
import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, withText, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Process (run, runCapture)
import Oglyad.Source.Cache (cacheDir)
import Oglyad.Types (SubmoduleEvent)
import System.Directory (createDirectory, createDirectoryIfMissing, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)

withScratchCopy :: FilePath -> (FilePath -> IO a) -> IO (Either Text a)
withScratchCopy root act = do
  base <- (</> "anda-work") <$> cacheDir
  createDirectoryIfMissing True base
  (tar, h) <- openBinaryTempFile base "tree.tar"
  hClose h
  let dir = tar <> ".d"
      cleanup = quietly (removeFile tar) >> quietly (removePathForcibly dir)
  ( do
      packed <- run "tar" ["-C", root, "--exclude=./.git", "-cf", tar, "."]
      unpacked <- case packed of
        Left e -> pure (Left e)
        Right _ -> createDirectory dir >> run "tar" ["-xf", tar, "-C", dir]
      quietly (removeFile tar)
      case unpacked of
        Left e -> pure (Left ("can't copy the tree for anda: " <> e))
        Right _ -> Right <$> act dir
    )
    `finally` cleanup
  where
    quietly :: IO () -> IO ()
    quietly a = () <$ (try a :: IO (Either SomeException ()))

-- | `anda list` prints lines like "anda/apps/foo/pkg (apps/foo)"
projectNames :: FilePath -> IO (Either Text (Map FilePath Text))
projectNames copy = do
  r <- runCapture copy [("NO_COLOR", "1")] "anda" ["list"]
  pure $ case r of
    Left e -> Left e
    Right (ExitFailure _, _, err) -> Left ("anda list failed: " <> lastLine err)
    Right (ExitSuccess, out, _) -> Right (Map.fromList (mapMaybe entry (T.lines (stripAnsi out))))
  where
    entry l = case T.breakOn " (" (T.strip l) of
      (path, rest)
        | Just name <- T.stripSuffix ")" (T.drop 2 rest),
          not (T.null path),
          not (T.null name) ->
            Just (takeDirectory (T.unpack path), name)
      _ -> Nothing

reportSupported :: IO Bool
reportSupported = do
  r <- runCapture "." [("NO_COLOR", "1")] "anda" ["update", "--help"]
  pure $ case r of
    Right (ExitSuccess, out, _) -> "--report" `T.isInfixOf` out
    _ -> False

-- | anda still exits 0 when a script fails, so the report is where failures
-- come from. the script gets GITHUB_TOKEN, since anda refuses gh() without one
updateProject :: FilePath -> Maybe Text -> Text -> Text -> IO (Either Text [SubmoduleEvent])
updateProject copy token branch name = do
  (report, h) <- openBinaryTempFile copy "anda-report.json"
  hClose h
  let env = ("NO_COLOR", "1") : [("GITHUB_TOKEN", T.unpack t) | Just t <- [token]]
  r <- runCapture copy env "anda" ["update", "--report", report, "--labels", "branch=" <> T.unpack branch, T.unpack name]
  case r of
    Left e -> pure (Left e)
    Right (ExitFailure _, _, err) -> pure (Left (failure (stripAnsi err)))
    Right (ExitSuccess, _, _) -> outcome . parseReport <$> BS.readFile report
  where
    -- color-eyre reports look like "Error:\n   0: Project not found: x"
    failure err = maybe (lastLine err) T.strip (listToMaybe [T.drop 3 l' | l <- T.lines err, let l' = T.stripStart l, "0: " `T.isPrefixOf` l'])
    outcome = \case
      Left e -> Left ("can't read anda's report: " <> e)
      Right [run'] -> case runStatus run' of
        Failed -> Left (maybe "update.rhai failed" describeError (runError run'))
        Panicked -> Left ("anda panicked" <> maybe "" ((": " <>) . errMessage) (runError run'))
        _ -> Right (runEvents run')
      Right runs -> Left ("anda's report has " <> T.pack (show (length runs)) <> " projects instead of one")

data RunStatus = Ran | Failed | Terminated | Panicked
  deriving stock (Eq, Show)

instance FromJSON RunStatus where
  parseJSON = withText "status" $ \case
    "ok" -> pure Ran
    "failed" -> pure Failed
    "terminated" -> pure Terminated
    "panicked" -> pure Panicked
    other -> fail ("unknown status " <> T.unpack other)

data RunError = RunError
  { errMessage :: Text,
    errFunction :: Maybe Text,
    errLine :: Maybe Int
  }
  deriving stock (Eq, Show)

instance FromJSON RunError where
  parseJSON = withObject "error" $ \o ->
    RunError <$> o .: "message" <*> o .:? "function" <*> o .:? "line"

data ProjectRun = ProjectRun
  { runProject :: Text,
    runStatus :: RunStatus,
    runError :: Maybe RunError,
    runEvents :: [SubmoduleEvent]
  }
  deriving stock (Eq, Show)

instance FromJSON ProjectRun where
  parseJSON = withObject "project" $ \o ->
    ProjectRun
      <$> o .: "project"
      <*> o .: "status"
      <*> o .:? "error"
      <*> (mapMaybe (parseMaybe parseJSON) <$> (o .:? "events" .!= ([] :: [Value])))

newtype Report = Report [ProjectRun]

instance FromJSON Report where
  parseJSON = withObject "report" $ \o -> do
    version <- o .: "version"
    if version == (1 :: Int)
      then Report <$> o .: "projects"
      else fail ("unsupported report version " <> show version)

parseReport :: BS.ByteString -> Either Text [ProjectRun]
parseReport = either (Left . T.pack) (\(Report runs) -> Right runs) . eitherDecodeStrict

describeError :: RunError -> Text
describeError RunError {..} =
  T.unwords (catMaybes [(<> "():") <$> errFunction, Just (oneLine errMessage), lineNote <$> errLine])
  where
    lineNote n = "(line " <> T.pack (show n) <> ")"
    oneLine = T.intercalate ", " . filter (not . T.null) . map T.strip . T.lines

-- | color-eyre writes ANSI color codes even with NO_COLOR set
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
  (pre, "") -> pre
  (pre, rest) -> pre <> stripAnsi (T.drop 1 (T.dropWhile (\c -> c /= 'm' && c /= 'K') (T.drop 2 rest)))

lastLine :: Text -> Text
lastLine t = case reverse (filter (not . T.null) (map T.strip (T.lines t))) of
  l : _ -> l
  [] -> "(no output)"
