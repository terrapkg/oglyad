module Oglyad.Check.Submodules (check) where

import Data.Text qualified as T
import Oglyad.Types

check :: Check
check _ p = map describe (pkgSubmoduleEvents p)
  where
    describe = \case
      SubmoduleAdded {..} ->
        finding "submodules" p Warn $
          "upstream added the submodule "
            <> sePath
            <> " ("
            <> seUrl
            <> " at "
            <> T.take 12 seCommit
            <> "). add %global "
            <> seGlobal
            <> " with a Source and %submodule_setup line, or pass it to rpm.submodules() as ignore"
      SubmoduleDropped {..} ->
        finding "submodules" p Warn $
          "upstream dropped the submodule for %global "
            <> seGlobal
            <> ". remove the global, its Source and its %submodule_setup line"
