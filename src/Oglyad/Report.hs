module Oglyad.Report
  ( renderText
  , renderJson
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Ord (Down (..))
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
        <> ": "
        <> fMessage
    count s = T.pack (show (length (filter ((== s) . fSeverity) fs)))
    summary = count Error <> " errors, " <> count Warn <> " warnings, " <> count Info <> " info"

renderJson :: [Finding] -> LBS.ByteString
renderJson = encode

sevText :: Severity -> Text
sevText = \case
  Info -> "info"
  Warn -> "warn"
  Error -> "error"
