module Oglyad.Check.BuildLag (check) where

import Oglyad.Types
import Oglyad.Version (compareEVR, renderEVR)

check :: Check
check _ p@Package {..} = case (pkgSpecEVR, pkgRepoBuild) of
  -- the failed repo fetch is already a collect error, so don't bury it under
  -- "never published" noise
  _ | not pkgRepoChecked -> []
  (Just spec, Nothing) ->
    [finding' Error ("never published (spec is at " <> renderEVR spec <> ")")]
  (Just spec, Just RepoBuild {rbEVR = repo})
    | spec == repo -> []
    | otherwise -> case compareEVR spec repo of
        GT -> [finding' Warn ("spec is at " <> renderEVR spec <> " but repo has " <> renderEVR repo)]
        LT -> [finding' Info ("repo has " <> renderEVR repo <> " but spec is at " <> renderEVR spec <> ", is the checkout up to date?")]
        -- spelled differently, e.g. 1.05 vs 1.5
        EQ -> []
  _ -> []
  where
    finding' = finding "build-lag" p
