-- | packages with no update.rhai have to be bumped by hand
module Oglyad.Check.NoAutoupdate (check) where

import Oglyad.Types

check :: Check
check _ p@Package {..} = case pkgUpstream of
  Nothing -> [finding "no-autoupdate" p Info "no update.rhai (or only comments in it), updates are manual"]
  Just _ -> []
