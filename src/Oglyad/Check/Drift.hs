-- | upstream has a newer version than the spec
module Oglyad.Check.Drift (check, drift) where

import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Types
import Oglyad.Version (compareUpstream)

-- | (upstream version, spec version, how upstream compares)
drift :: Package -> Maybe (Text, Text, Ordering)
drift Package {..} = do
  up <- T.strip <$> pkgUpstreamVer
  spec <- evrVersion <$> pkgSpecEVR
  pure (up, spec, compareUpstream up spec)

-- pkgUpstreamVer is only populated with --upstream, so this check is silent
-- by default
check :: Check
check _ p@Package {..} = case drift p of
  Just (up, spec, GT) -> [finding' Warn ("upstream has " <> up <> ", spec is at " <> spec)]
  Just (up, spec, LT)
    -- spec ahead of upstream. snapshots (Version: 1.2^20251101git...) built
    -- after the 1.2 release always look like this and are fine. anything else
    -- usually means a tag that normalizes badly
    | T.any (== '^') spec -> []
    | otherwise -> [finding' Info ("spec " <> spec <> " is ahead of upstream " <> up <> ", check tag normalization")]
  _ -> []
  where
    finding' = finding "drift" p
