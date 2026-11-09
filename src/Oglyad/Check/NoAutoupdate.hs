-- | packages with no update.rhai have to be bumped by hand
module Oglyad.Check.NoAutoupdate (check) where

import Oglyad.Types

check :: Check
check _ Package {..} = case pkgUpstream of
  Nothing -> [Finding "no-autoupdate" pkgName Info "no update.rhai, updates are manual"]
  Just _ -> []
