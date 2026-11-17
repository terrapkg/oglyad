-- | a package should live in Terra or its base distro, not both
module Oglyad.Check.FedoraOverlap (check) where

import Data.Set qualified as Set
import Oglyad.Types

-- | subrepos (extras, mesa, multimedia, nvidia) exist to ship replacements
-- for base distro packages, so overlap there is the whole point
check :: Check
check Snapshot {..} p@Package {..}
  | Just _ <- pkgSubrepo = []
  | pkgName `Set.member` snapBase =
      [finding "fedora-overlap" p Warn ("also shipped by " <> cfgBaseName snapConfig)]
  | otherwise = []
