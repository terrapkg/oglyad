-- | a package should live in Terra or Fedora, not both
module Oglyad.Check.FedoraOverlap (check) where

import Data.Set qualified as Set
import Oglyad.Types

-- | subrepos (extras, mesa, multimedia, nvidia) exist to ship replacements
-- for Fedora packages, so overlap there is the whole point
check :: Check
check Snapshot {..} Package {..}
  | Just _ <- pkgSubrepo = []
  | pkgName `Set.member` snapFedora =
      [Finding "fedora-overlap" pkgName Warn "also shipped by Fedora"]
  | otherwise = []
