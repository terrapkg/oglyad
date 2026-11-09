-- | what's merged in git doesn't match what's published
module Oglyad.Check.BuildLag (check) where

import Oglyad.Types
import Oglyad.Version (compareEVR, renderEVR)

check :: Check
check _ p@Package {..} = case (pkgSpecEVR, pkgRepoBuild) of
  (Just spec, Nothing) ->
    [finding' Error ("never published (spec is at " <> renderEVR spec <> ")")]
  (Just spec, Just RepoBuild {rbEVR = repo})
    | spec == repo -> []
    | otherwise -> case compareEVR spec repo of
        -- merged but not built: the build failed or is still queued
        GT -> [finding' Warn ("spec is at " <> renderEVR spec <> " but repo has " <> renderEVR repo)]
        -- the repo is newer than git, so the monorepo checkout is behind
        LT -> [finding' Info ("repo has " <> renderEVR repo <> " but spec is at " <> renderEVR spec <> ", is the checkout up to date?")]
        -- same version spelled differently, e.g. 1.05 vs 1.5
        EQ -> []
  _ -> []
  where
    finding' = finding "build-lag" p
