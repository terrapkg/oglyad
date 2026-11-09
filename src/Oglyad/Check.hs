-- | the check registry. adding a check = write a module, add a line here
module Oglyad.Check
  ( allChecks
  , runChecks
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Oglyad.Check.BuildLag qualified as BuildLag
import Oglyad.Check.Drift qualified as Drift
import Oglyad.Check.FedoraOverlap qualified as FedoraOverlap
import Oglyad.Check.NoAutoupdate qualified as NoAutoupdate
import Oglyad.Check.Stale qualified as Stale
import Oglyad.Check.UnparsedRhai qualified as UnparsedRhai
import Oglyad.Check.UpstreamError qualified as UpstreamError
import Oglyad.Types

allChecks :: [(Text, Check)]
allChecks =
  [ ("build-lag", BuildLag.check)
  , ("drift", Drift.check)
  , ("fedora-overlap", FedoraOverlap.check)
  , ("no-autoupdate", NoAutoupdate.check)
  , ("stale", Stale.check)
  , ("unparsed-rhai", UnparsedRhai.check)
  , ("upstream-error", UpstreamError.check)
  ]

-- | run the named checks (all of them when the list is empty) over every package
runChecks :: [Text] -> Snapshot -> [Finding]
runChecks only snap =
  [ f
  | (name, chk) <- allChecks
  , null only || name `elem` only
  , pkg <- Map.elems (snapPackages snap)
  , f <- chk snap pkg
  ]
