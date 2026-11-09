module Oglyad.Report
  ( renderText,
    renderJson,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Types

renderText :: Severity -> [Finding] -> Text
renderText minSev fs =
  T.unlines (map line shown <> ["", summary])
  where
    shown = sortOn (\f -> (Down (fSeverity f), fCheck f, fPackage f)) (filter ((>= minSev) . fSeverity) fs)
    line Finding {..} =
      T.justifyLeft 6 ' ' (sevText fSeverity)
        <> T.justifyLeft 16 ' ' fCheck
        <> fPackage
        <> maybe "" (\path -> if fPackage `Set.member` ambiguous then " (" <> path <> ")" else "") fPath
        <> ": "
        <> fMessage
    -- names that show up with more than one directory
    ambiguous =
      Map.keysSet . Map.filter ((> 1) . Set.size) $
        Map.fromListWith (<>) [(fPackage f, Set.fromList (maybe [] pure (fPath f))) | f <- fs]
    count s = T.pack (show (length (filter ((== s) . fSeverity) fs)))
    summary = count Error <> " errors, " <> count Warn <> " warnings, " <> count Info <> " info"

renderJson :: [Finding] -> LBS.ByteString
renderJson = encode

sevText :: Severity -> Text
sevText = \case
  Info -> "info"
  Warn -> "warn"
  Error -> "error"
