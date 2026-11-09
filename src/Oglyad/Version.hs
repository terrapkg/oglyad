-- | RPM version handling: EVR parsing and the rpmvercmp algorithm
--
-- reference implementation: rpmio/rpmvercmp.c in rpm-software-management/rpm
-- test vectors: tests/rpmvercmp.at in the same repo
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
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

-- | epoch:version-release
--
-- note: 'Eq' is structural but 'Ord' goes through rpmvercmp, so "1.05" and
-- "1.5" compare EQ while being /=. build-lag relies on the structural Eq on
-- purpose, since any textual difference between git and the repo is worth
-- reporting
data EVR = EVR
  { evrEpoch :: !Int,
    evrVersion :: !Text,
    evrRelease :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord EVR where
  compare = compareEVR

-- | parse "[epoch:]version-release". a missing epoch or "(none)" means 0
parseEVR :: Text -> Maybe EVR
parseEVR raw = do
  let (epochPart, rest) = case T.breakOn ":" raw of
        (vr, "") -> ("0", vr)
        (e, r) -> (e, T.drop 1 r)
  epoch <- if epochPart == "(none)" then Just 0 else readMaybe (T.unpack epochPart)
  -- release is everything after the *last* dash, versions can't contain dashes
  let (verDash, rel) = T.breakOnEnd "-" rest
  ver <- T.stripSuffix "-" verDash
  if T.null ver || T.null rel then Nothing else Just (EVR epoch ver rel)

renderEVR :: EVR -> Text
renderEVR (EVR 0 v r) = v <> "-" <> r
renderEVR (EVR e v r) = T.pack (show e) <> ":" <> v <> "-" <> r

compareEVR :: EVR -> EVR -> Ordering
compareEVR (EVR e1 v1 r1) (EVR e2 v2 r2) =
  compare e1 e2 <> rpmvercmp v1 v2 <> rpmvercmp r1 r2

-- | compare two version (or release) strings exactly the way rpm does.
-- this is a line-by-line port of rpmvercmp.c
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
                -- tilde: whoever has it is older
                (Just ('~', one'), Just ('~', two')) -> go one' two'
                (Just ('~', _), _) -> LT
                (_, Just ('~', _)) -> GT
                -- caret: older than anything except the end of the string
                (Just ('^', one'), Just ('^', two')) -> go one' two'
                (Nothing, Just ('^', _)) -> LT
                (Just ('^', _), Nothing) -> GT
                (Just ('^', _), Just _) -> LT
                (Just _, Just ('^', _)) -> GT
                -- one side ran out
                (Nothing, Nothing) -> EQ
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Just (c, _), Just _)
                  | isDigit c -> segment True (T.span isDigit one) (T.span isDigit two)
                  | otherwise -> segment False (T.span isAlpha one) (T.span isAlpha two)

    segment isNum (seg1, rest1) (seg2, rest2)
      -- the other side has a different kind of segment here.
      -- numeric beats alpha
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
  let t = T.takeWhileEnd (/= '/') (T.strip raw)
      noV = maybe t id (T.stripPrefix "v" t)
      fromDigit = case T.dropWhile (not . isDigit) noV of
        "" -> noV
        d -> d
   in T.replace "-" "." (tildePre fromDigit)

-- | "-rc1" / ".beta" / "_alpha2" → "~rc1" / "~beta" / "~alpha2"
tildePre :: Text -> Text
tildePre t = case T.uncons t of
  Nothing -> ""
  Just (c, rest)
    | c `elem` ("-._" :: String), Just _ <- marker rest -> T.cons '~' (tildePre rest)
    | otherwise -> T.cons c (tildePre rest)
  where
    marker r = find (`T.isPrefixOf` T.toLower r) ["alpha", "beta", "rc", "pre", "dev"]

-- | compare an upstream version with a spec Version, forgiving the usual
-- ways packagers respell a version. returns how upstream compares to the spec
--
--   * same letters and digits, different punctuation is the same version
--     (0.4.2 vs 0.4~2, 2026.07.27 vs 20260727)
--   * the same leading dotted number with junk after it is the same version
--     (2.005R vs 2.005, 3.17.0-ogc1 vs 3.17.0^1), unless one side is a
--     prerelease, since 6.0.0 vs 6.0.0~alpha1 is real drift
--   * otherwise rpmvercmp decides
compareUpstream :: Text -> Text -> Ordering
compareUpstream up spec
  | squash u == squash s = EQ
  | not (T.null (core u)) && core u == core s && not (isPre u || isPre s) = EQ
  | otherwise = rpmvercmp u s
  where
    u = normalizeVersion up
    s = normalizeVersion spec
    squash = T.toLower . T.filter isAlphaNum
    core = T.dropWhileEnd (== '.') . T.takeWhile (\c -> isDigit c || c == '.')
    -- a prerelease is never "the same version with junk after it"
    isPre v = "~" `T.isPrefixOf` T.drop (T.length (core v)) (T.dropWhileEnd (== '.') v)
