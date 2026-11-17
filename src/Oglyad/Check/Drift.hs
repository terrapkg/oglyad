module Oglyad.Check.Drift (check, drift) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Types
import Oglyad.Version (compareUpstream)

-- | (upstream, spec, ordering) for packages meant to follow upstream releases.
-- nightlies only borrow the last release as a base, so they can't lag behind
-- it. `weekly` packages still count, those are mostly releases checked less often
drift :: Package -> Maybe (Text, Text, Ordering)
drift p
  | pkgNightly p = Nothing
  | otherwise = compared p

compared :: Package -> Maybe (Text, Text, Ordering)
compared Package {..} = do
  up <- T.strip <$> pkgUpstreamVer
  spec <- evrVersion <$> pkgSpecEVR
  pure (up, spec, compareUpstream up spec)

check :: Check
check _ p = case compared p of
  -- usually a crop(1) meant for a "v" prefix the tag doesn't have, turning
  -- 6.0.1 into .0.1
  Just (up, _, _)
    | not (startsWithAlphaNum up) ->
        [finding' Warn ("update.rhai would set the version to \"" <> up <> "\", which doesn't start with a letter or digit")]
  _ -> case drift p of
    Just r -> compareFindings r
    Nothing -> []
  where
    finding' = finding "drift" p
    startsWithAlphaNum = maybe False (isAlphaNum . fst) . T.uncons
    isSnapshot = T.any (== '^')
    compareFindings = \case
      (up, spec, GT) -> [finding' Warn ("upstream has " <> up <> ", spec is at " <> spec)]
      (up, spec, LT)
        -- a snapshot like 1.2^20251101git built after the 1.2 release always
        -- sorts ahead of it
        | isSnapshot spec -> []
        | otherwise -> [finding' Info ("spec " <> spec <> " is ahead of upstream " <> up <> ", check tag normalization")]
      _ -> []
