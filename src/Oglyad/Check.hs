-- | the check registry. adding a check = write a module, add a line here
module Oglyad.Check
  ( allChecks,
    checkNames,
    parseCheckName,
    runChecks,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Check.BuildLag qualified as BuildLag
import Oglyad.Check.Drift qualified as Drift
import Oglyad.Check.DuplicateName qualified as DuplicateName
import Oglyad.Check.FedoraOverlap qualified as FedoraOverlap
import Oglyad.Check.NoAutoupdate qualified as NoAutoupdate
import Oglyad.Check.Stale qualified as Stale
import Oglyad.Check.UnparsedRhai qualified as UnparsedRhai
import Oglyad.Check.UpstreamError qualified as UpstreamError
import Oglyad.Types

allChecks :: [(Text, Check)]
allChecks =
  [ ("build-lag", BuildLag.check),
    ("drift", Drift.check),
    ("duplicate-name", DuplicateName.check),
    ("fedora-overlap", FedoraOverlap.check),
    ("no-autoupdate", NoAutoupdate.check),
    ("stale", Stale.check),
    ("unparsed-rhai", UnparsedRhai.check),
    ("upstream-error", UpstreamError.check)
  ]

checkNames :: [Text]
checkNames = map fst allChecks

-- | validate a check name from the command line. a typo has to be an error,
-- otherwise `--only buildlag` quietly runs nothing and reports all clear
parseCheckName :: String -> Either String Text
parseCheckName raw
  | name `elem` checkNames = Right name
  | otherwise =
      Left ("unknown check " <> show raw <> ", expected one of: " <> T.unpack (T.intercalate ", " checkNames))
  where
    name = T.pack raw

-- | run the named checks (all of them when the list is empty) over every package
runChecks :: [Text] -> Snapshot -> [Finding]
runChecks only snap =
  [ f
  | (name, chk) <- allChecks,
    null only || name `elem` only,
    pkg <- Map.elems (snapPackages snap),
    f <- chk snap pkg
  ]
