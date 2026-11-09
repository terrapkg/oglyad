-- | running the real andax on update.rhai scripts oglyad can't follow on its
-- own, like ones that scrape a page with get() and find().
--
-- anda rewrites specs in place, so it runs on a throwaway copy of the tree.
-- afterwards rpmspec reads the copy's spec, and whatever Version it says now
-- is what `anda update` would have set
module Oglyad.Source.Anda
  ( withScratchCopy,
    projectNames,
    updateProject,
    scriptErrors,
    stripAnsi,
  )
where

import Control.Exception (SomeException, finally, try)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Process (run, runCapture)
import Oglyad.Source.Cache (cacheDir)
import System.Directory (createDirectory, createDirectoryIfMissing, removeFile, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)

-- | run an action on a copy of a tree (minus .git) that's deleted afterwards
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

-- | package directory to anda project name, from `anda list`, which prints
-- lines like "anda/apps/foo/pkg (apps/foo)"
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

-- | `anda update` one project in the copy. anda reports a failing script
-- only in its log and still exits 0, so the log is where errors come from.
-- the script gets GITHUB_TOKEN, since anda refuses gh() without one
updateProject :: FilePath -> Maybe Text -> Text -> Text -> IO (Either Text ())
updateProject copy token branch name = do
  let env = ("NO_COLOR", "1") : [("GITHUB_TOKEN", T.unpack t) | Just t <- [token]]
  r <- runCapture copy env "anda" ["-v", "update", "--labels", "branch=" <> T.unpack branch, T.unpack name]
  pure $ case r of
    Left e -> Left e
    Right (ExitFailure _, _, err) -> Left (failure (stripAnsi err))
    Right (ExitSuccess, _, err) -> case scriptErrors (stripAnsi err) of
      [] -> Right ()
      e : _ -> Left e
  where
    -- color-eyre reports look like "Error:\n   0: Project not found: x"
    failure err = maybe (lastLine err) T.strip (listToMaybe [T.drop 3 l' | l <- T.lines err, let l' = T.stripStart l, "0: " `T.isPrefixOf` l'])

-- | what went wrong in each script exception in anda's -v log: andax logs
-- "ERROR andax::run: Script Exception —— <project>" and then the reason on a
-- line starting with "├─═"
scriptErrors :: Text -> [Text]
scriptErrors = go . T.lines
  where
    go = \case
      [] -> []
      l : rest
        | "Script Exception" `T.isInfixOf` l ->
            case [T.strip (T.drop 1 (snd (T.breakOn "═" r))) | r <- take 5 rest, "├─═" `T.isInfixOf` r] of
              msg : _ -> msg : go rest
              [] -> "script exception" : go rest
        | otherwise -> go rest

-- | drop ANSI color codes, which color-eyre writes even with NO_COLOR set
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
  (pre, "") -> pre
  (pre, rest) -> pre <> stripAnsi (T.drop 1 (T.dropWhile (\c -> c /= 'm' && c /= 'K') (T.drop 2 rest)))

lastLine :: Text -> Text
lastLine t = case reverse (filter (not . T.null) (map T.strip (T.lines t))) of
  l : _ -> l
  [] -> "(no output)"
