-- | packages that haven't been built in a long time
module Oglyad.Check.Stale (check) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, nominalDay)
import Oglyad.Check.Drift (drift)
import Oglyad.Types

-- | a stable package nobody touched for a year might be totally fine, so an
-- old build alone is Info. it only becomes a Warn when upstream has moved on
-- (needs --upstream) and the package is still sitting on the old version
check :: Check
check Snapshot {..} p@Package {..} = case pkgRepoBuild of
  Nothing -> [] -- never published is build-lag's job
  Just RepoBuild {..}
    | diffUTCTime snapNow rbBuildTime < cfgStaleAfter snapConfig -> []
    | otherwise -> [finding "stale" p sev msg]
    where
      upstreamMoved = case drift p of
        Just (_, _, GT) -> True
        _ -> False
      sev = if upstreamMoved then Warn else Info
      msg =
        "last built "
          <> ago rbBuildTime
          <> maybe "" (\c -> ", last commit " <> ago c) pkgLastCommit
          <> (if upstreamMoved then ", and upstream has a newer version" else "")
  where
    ago :: UTCTime -> Text
    ago t = T.pack (show (floor (diffUTCTime snapNow t / nominalDay) :: Int)) <> " days ago"
