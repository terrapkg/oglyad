-- | the update.rhai source parsed fine but asking upstream failed. usually a
-- renamed or deleted repo, which means `anda update` is failing on it too
module Oglyad.Check.UpstreamError (check) where

import Oglyad.Types

check :: Check
check _ p@Package {..} = case pkgUpstreamErr of
  Just err -> [finding "upstream-error" p Warn err]
  Nothing -> []
