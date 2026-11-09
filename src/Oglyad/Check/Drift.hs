-- | upstream has a newer version than the spec
module Oglyad.Check.Drift (check, drift) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Types
import Oglyad.Version (compareUpstream)

-- | (upstream version, spec version, how upstream compares), for packages
-- whose version is meant to follow upstream releases. nightlies (a `nightly`
-- or `quaterdaily` label in anda.hcl) build from a branch head and only
-- borrow the last release as a base, so the newest release isn't something
-- they lag behind. `weekly` isn't one of them: those are mostly releases
-- (fonts, nvidia) checked less often
drift :: Package -> Maybe (Text, Text, Ordering)
drift p
  | pkgNightly p = Nothing
  | otherwise = compared p

compared :: Package -> Maybe (Text, Text, Ordering)
compared Package {..} = do
  up <- T.strip <$> pkgUpstreamVer
  spec <- evrVersion <$> pkgSpecEVR
  pure (up, spec, compareUpstream up spec)

-- pkgUpstreamVer is only populated with --upstream, so this check is silent
-- by default
check :: Check
check _ p = case compared p of
  -- usually a crop(1) meant for a "v" prefix on a tag that doesn't have one,
  -- which turns 6.0.1 into .0.1. a bug in the script, nightly or not
  Just (up, _, _)
    | maybe True (not . isAlphaNum . fst) (T.uncons up) ->
        [finding' Warn ("update.rhai would set the version to \"" <> up <> "\", which doesn't start with a letter or digit")]
  _ -> case drift p of
    Just r -> compareFindings r
    Nothing -> []
  where
    finding' = finding "drift" p
    compareFindings = \case
      (up, spec, GT) -> [finding' Warn ("upstream has " <> up <> ", spec is at " <> spec)]
      (up, spec, LT)
        -- spec ahead of upstream. snapshots (Version: 1.2^20251101git...)
        -- built after the 1.2 release always look like this and are fine.
        -- anything else usually means a tag that normalizes badly
        | T.any (== '^') spec -> []
        | otherwise -> [finding' Info ("spec " <> spec <> " is ahead of upstream " <> up <> ", check tag normalization")]
      _ -> []
