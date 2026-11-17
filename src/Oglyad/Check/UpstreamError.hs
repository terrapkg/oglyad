module Oglyad.Check.UpstreamError (check) where

import Oglyad.Types

check :: Check
check _ p@Package {..} = case pkgUpstreamErr of
  Just err -> [finding "upstream-error" p Warn err]
  Nothing -> []
