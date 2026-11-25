module Oglyad.Source.Rozbir
  ( Evaluator,
    Mismatch (..),
    parseRozbirMode,
    newEvaluator,
    evaluateSpec,
    mismatches,
    versionAfterGlobal,
  )
where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Source.Monorepo (SpecInfo (..), querySpec)
import Oglyad.Types (RozbirMode (..))
import Oglyad.Version (renderEVR)
import Rozbir (ResolveOptions (..), Resolved (..), Target (..), evr, name, newEnv, parse, resolve, resolveWith, version)
import Rozbir.Edit (apply, setMacro)
import Rozbir.Eval (Env)
import Rozbir.IO (loadSystemMacros, readSpec, resolveIncludes)
import System.FilePath (takeDirectory)
import System.Info qualified

data Evaluator = Evaluator
  { evMode :: RozbirMode,
    evEnv :: Env,
    evMismatches :: IORef [Mismatch]
  }

data Mismatch = Mismatch
  { mmSpec :: FilePath,
    mmRozbir :: Text,
    mmRpmspec :: Text
  }
  deriving stock (Eq, Show)

parseRozbirMode :: String -> Either String RozbirMode
parseRozbirMode = \case
  "off" -> Right RozbirOff
  "shadow" -> Right RozbirShadow
  "first" -> Right RozbirFirst
  other -> Left ("unknown rozbir mode " <> other <> ", use off, shadow or first")

newEvaluator :: RozbirMode -> IO Evaluator
newEvaluator mode = do
  env <- if mode == RozbirOff then pure (newEnv True) else loadSystemMacros (Just (T.pack System.Info.arch))
  Evaluator mode env <$> newIORef []

viaRozbir :: Env -> [(Text, Maybe Text)] -> FilePath -> IO (Maybe SpecInfo)
viaRozbir env macros spec = do
  r <- try $ do
    tree <- readSpec spec
    includes <- resolveIncludes spec tree
    let target = Target (Just (T.pack System.Info.arch)) (Just "linux") (macros <> [("_sourcedir", Just (T.pack (takeDirectory spec)))])
        resolved = resolveWith (ResolveOptions env target includes) tree
    pure $ case (name resolved, evr resolved) of
      (Resolved n, Resolved e) -> Just (SpecInfo n e)
      _ -> Nothing
  pure (either (\(_ :: SomeException) -> Nothing) id r)

evaluateSpec :: Evaluator -> [(Text, Maybe Text)] -> FilePath -> IO (Either Text SpecInfo)
evaluateSpec Evaluator {..} macros spec = case evMode of
  RozbirOff -> querySpec macros spec
  RozbirFirst -> viaRozbir evEnv macros spec >>= maybe (querySpec macros spec) (pure . Right)
  RozbirShadow -> do
    theirs <- querySpec macros spec
    ours <- viaRozbir evEnv macros spec
    case (ours, theirs) of
      (Just o, Right t)
        | o /= t ->
            atomicModifyIORef' evMismatches (\ms -> (Mismatch spec (describe o) (describe t) : ms, ()))
      _ -> pure ()
    pure theirs
  where
    describe SpecInfo {..} = siName <> " " <> renderEVR siEVR

mismatches :: Evaluator -> IO [Mismatch]
mismatches ev = reverse . Map.elems . Map.fromList . map (\m -> (mmSpec m, m)) <$> readIORef (evMismatches ev)

versionAfterGlobal :: [(Text, Maybe Text)] -> Text -> Text -> Text -> Maybe Text
versionAfterGlobal macros key value spec = case setMacro key value (parse spec) of
  Left _ -> Nothing
  Right edits ->
    let tree = parse (apply edits spec)
        target = Target (Just (T.pack System.Info.arch)) (Just "linux") macros
     in case version (resolve (newEnv False) target tree) of
          Resolved v -> Just v
          Unresolved _ -> Nothing
