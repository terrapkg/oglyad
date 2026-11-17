module Oglyad.Check.FedoraOverlap (check) where

import Data.Set qualified as Set
import Oglyad.Types

-- | subrepos (extras, mesa, multimedia, nvidia) exist to replace base distro
-- packages
check :: Check
check Snapshot {..} p@Package {..}
  | Just _ <- pkgSubrepo = []
  | pkgName `Set.member` snapBase =
      [finding "fedora-overlap" p Warn ("also shipped by " <> cfgBaseName snapConfig)]
  | otherwise = []
