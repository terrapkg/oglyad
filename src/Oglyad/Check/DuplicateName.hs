-- | two monorepo directories building the same source package into the same
-- repo. only one build can be live in a repo, so they overwrite each other,
-- and nothing (including build-lag) can tell which one a published rpm came
-- from. sometimes deliberate (a bootstrap build, a 32-bit variant), sometimes
-- a copy-pasted Name: that was never changed
module Oglyad.Check.DuplicateName (check) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Oglyad.Types

check :: Check
check snap p = case others of
  [] -> []
  _ ->
    [ finding "duplicate-name" p Warn $
        "also built by "
          <> T.intercalate ", " (map (T.pack . pkgPath) others)
          <> ", and both publish to the same repo"
    ]
  where
    others =
      [ q
      | q <- Map.elems (snapPackages snap),
        pkgName q == pkgName p,
        pkgSubrepo q == pkgSubrepo p,
        pkgPath q /= pkgPath p
      ]
