-- | ports rpmio/rpmvercmp.c from rpm-software-management/rpm, tested
-- against its tests/rpmvercmp.at
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- | 'Eq' is structural but 'Ord' goes through rpmvercmp, so "1.05" and "1.5"
-- compare EQ while being /=. build-lag relies on that, since any textual
-- difference between git and the repo is worth reporting
data EVR = EVR
  { evrEpoch :: !Int,
    evrVersion :: !Text,
    evrRelease :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord EVR where
  compare = compareEVR

parseEVR :: Text -> Maybe EVR
parseEVR raw = do
  let (epochPart, rest) = case T.breakOn ":" raw of
        (vr, "") -> ("0", vr)
        (e, r) -> (e, T.drop 1 r)
  epoch <- if epochPart == "(none)" then Just 0 else readMaybe (T.unpack epochPart)
  -- versions can't contain dashes, so the release starts after the last one
  let (verDash, rel) = T.breakOnEnd "-" rest
  ver <- T.stripSuffix "-" verDash
  if T.null ver || T.null rel then Nothing else Just (EVR epoch ver rel)

renderEVR :: EVR -> Text
renderEVR (EVR 0 v r) = v <> "-" <> r
renderEVR (EVR e v r) = T.pack (show e) <> ":" <> v <> "-" <> r

compareEVR :: EVR -> EVR -> Ordering
compareEVR (EVR e1 v1 r1) (EVR e2 v2 r2) =
  compare e1 e2 <> rpmvercmp v1 v2 <> rpmvercmp r1 r2

-- | a line-by-line port of rpmvercmp.c
--
--   * anything that isn't an ascii letter, digit, '~' or '^' is a separator
--   * '~' sorts before everything, even the end of the string (1.0~rc1 < 1.0)
--   * '^' sorts after the end of the string but before anything else
--     (1.0 < 1.0^git1 < 1.0.1)
--   * otherwise compare segment by segment, where a segment is a run of
--     digits or a run of letters. numbers beat letters, numbers compare
--     numerically, letters compare lexically
--   * when one side runs out, the side with stuff left is newer
rpmvercmp :: Text -> Text -> Ordering
rpmvercmp a b
  | a == b = EQ
  | otherwise = go a b
  where
    go one0 two0
      | T.null one0 && T.null two0 = EQ
      | otherwise =
          let one = T.dropWhile isSep one0
              two = T.dropWhile isSep two0
           in case (T.uncons one, T.uncons two) of
                (Just ('~', one'), Just ('~', two')) -> go one' two'
                (Just ('~', _), _) -> LT
                (_, Just ('~', _)) -> GT
                (Just ('^', one'), Just ('^', two')) -> go one' two'
                (Nothing, Just ('^', _)) -> LT
                (Just ('^', _), Nothing) -> GT
                (Just ('^', _), Just _) -> LT
                (Just _, Just ('^', _)) -> GT
                (Nothing, Nothing) -> EQ
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Just (c, _), Just _)
                  | isDigit c -> segment True (T.span isDigit one) (T.span isDigit two)
                  | otherwise -> segment False (T.span isAlpha one) (T.span isAlpha two)

    segment isNum (seg1, rest1) (seg2, rest2)
      | T.null seg2 = if isNum then GT else LT
      | isNum =
          let n1 = T.dropWhile (== '0') seg1
              n2 = T.dropWhile (== '0') seg2
           in compare (T.length n1) (T.length n2) <> compare n1 n2 <> go rest1 rest2
      | otherwise = compare seg1 seg2 <> go rest1 rest2

    isAlpha c = isAsciiLower c || isAsciiUpper c
    isSep c = not (isDigit c || isAlpha c || c == '~' || c == '^')

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
