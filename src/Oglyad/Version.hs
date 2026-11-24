module Oglyad.Version
  ( EVR (..),
    parseEVR,
    renderEVR,
    rpmvercmp,
    compareEVR,
    normalizeVersion,
    compareUpstream,
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Rozbir.Version (EVR (..), compareEVR, parseEVR, renderEVR, rpmvercmp)

-- | turn an upstream tag (or a sloppy spec Version) into something
-- rpmvercmp can compare. starts from what anda's rpm.version() does (trim,
-- drop a leading v, dashes become dots) and adds:
--
--   * monorepo tags like "pkg/v1.2" keep only the part after the last slash
--   * non-numeric prefixes go, so "release-1.2" and "jq-1.7" line up
--   * prerelease markers become tildes like Fedora spells them,
--     so "6.0.0-alpha1" becomes "6.0.0~alpha1"
normalizeVersion :: Text -> Text
normalizeVersion raw =
  let afterLastSlash = T.takeWhileEnd (/= '/') (T.strip raw)
      withoutV = fromMaybe afterLastSlash (T.stripPrefix "v" afterLastSlash)
      fromFirstDigit = case T.dropWhile (not . isDigit) withoutV of
        "" -> withoutV
        d -> d
   in T.replace "-" "." (tildePrereleases fromFirstDigit)

tildePrereleases :: Text -> Text
tildePrereleases t = case T.uncons t of
  Nothing -> ""
  Just (c, rest)
    | c `elem` ("-._" :: String), startsWithPrerelease rest -> T.cons '~' (tildePrereleases rest)
    | otherwise -> T.cons c (tildePrereleases rest)
  where
    startsWithPrerelease r = any (`T.isPrefixOf` T.toLower r) ["alpha", "beta", "rc", "pre", "dev"]

-- | compare an upstream version with a spec Version, forgiving the usual
-- ways packagers respell a version. returns how upstream compares to the spec
--
--   * same letters and digits, different punctuation is the same version
--     (0.4.2 vs 0.4~2, 2026.07.27 vs 20260727). a snapshot suffix on the
--     spec (^20260805git..., .gitfc62ae7) doesn't count, since a snapshot of
--     the release upstream just tagged isn't behind it
--   * the same leading dotted number with junk after it is the same version
--     (2.005R vs 2.005, 3.17.0-ogc1 vs 3.17.0^1), unless one side is a
--     prerelease, since 6.0.0 vs 6.0.0~alpha1 is real drift
--   * otherwise rpmvercmp decides
compareUpstream :: Text -> Text -> Ordering
compareUpstream up spec
  | sameAlnums = EQ
  | sameLeadingNumber && not (isPrerelease u || isPrerelease s) = EQ
  | otherwise = rpmvercmp u s
  where
    u = normalizeVersion up
    s = normalizeVersion spec
    sameAlnums = alnums u `elem` [alnums s, alnums (snapshotBase s)]
    sameLeadingNumber = not (T.null (leadingNumber u)) && leadingNumber u == leadingNumber s
    alnums = T.toLower . T.filter isAlphaNum
    leadingNumber = T.dropWhileEnd (== '.') . T.takeWhile (\c -> isDigit c || c == '.')
    isPrerelease v = "~" `T.isPrefixOf` T.drop (T.length (leadingNumber v)) (T.dropWhileEnd (== '.') v)

-- | "~git" isn't a snapshot marker, since 2.0~git... is a snapshot from
-- before the release and really is behind 2.0
snapshotBase :: Text -> Text
snapshotBase v =
  let base = T.takeWhile (/= '^') v
      cuts = [pre | sep <- [".git", "+git", "_git"], let (pre, post) = T.breakOn sep base, not (T.null post)]
   in minimumBy (comparing T.length) (base : cuts)
