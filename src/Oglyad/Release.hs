-- | which Fedora releases to check, and everything that follows from one and
-- the distro being checked: the repo, the %{dist} tag, the monorepo branch
-- and the Fedora repos. also merges findings from several releases into one
-- report
module Oglyad.Release
  ( Distro (..),
    terra,
    ultramarine,
    distros,
    lookupDistro,
    Release (..),
    isRawhide,
    releaseLabel,
    releaseDist,
    releaseBranch,
    repoName,
    defaultRepoUrl,
    repoUrlFor,
    fedoraReposFor,
    parseReleaseArg,
    ReleaseArg (..),
    bodhiReleasesUrl,
    parseBodhiReleases,
    mergeFindings,
  )
where

import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Oglyad.Types (Finding (..))
import Text.Read (readMaybe)

-- | an anda monorepo oglyad knows how to check, and how it spells things per
-- release
data Distro = Distro
  { -- | what --distro takes, e.g. "terra"
    distroName :: Text,
    -- | the directory under the monorepo root holding every package, e.g.
    -- "anda" for terrapkg/packages
    distroRoot :: FilePath,
    -- | the repo's name, with the release name appended: terra for terra43
    distroRepo :: Text,
    -- | release branch prefix, f for f43
    distroBranch :: Text,
    -- | %{dist} prefix, .fc for .fc43
    distroDist :: Text
  }
  deriving stock (Eq, Show)

-- | terrapkg/packages
terra :: Distro
terra = Distro "terra" "anda" "terra" "f" ".fc"

-- | Ultramarine-Linux/packages. builds land in um43, umrawhide, with
-- .um43 and .umrawhide dist tags
ultramarine :: Distro
ultramarine = Distro "ultramarine" "ultramarine" "um" "um" ".um"

distros :: [Distro]
distros = [terra, ultramarine]

lookupDistro :: String -> Either String Distro
lookupDistro raw = case [d | d <- distros, distroName d == T.pack raw] of
  d : _ -> Right d
  [] -> Left ("unknown distro " <> show raw <> ", expected one of: " <> T.unpack (T.intercalate ", " (map distroName distros)))

-- | a Fedora release Terra builds for. rawhide keeps its number too, since
-- %fedora still needs one when evaluating specs
data Release = Release
  { -- | "43", or "rawhide"
    relName :: Text,
    -- | the number %fedora expands to
    relVersion :: Int
  }
  deriving stock (Eq, Show)

isRawhide :: Release -> Bool
isRawhide r = relName r == "rawhide"

-- | how releases are spelled in output: f43, rawhide. the same for every
-- distro, since they're all built on Fedora's releases
releaseLabel :: Release -> Text
releaseLabel r = if isRawhide r then "rawhide" else "f" <> relName r

-- | rawhide builds use .fcrawhide, not .fc46. terrapkg/packages' bootstrap
-- workflow rewrites macros.dist to do it, and Ultramarine does the same
releaseDist :: Distro -> Release -> Text
releaseDist d r = distroDist d <> relName r

-- | the monorepo branch for a release: f43, frawhide, um43
releaseBranch :: Distro -> Release -> Text
releaseBranch d r = distroBranch d <> relName r

-- | the distro's repo for a release, e.g. terra43 or umrawhide. subrepos
-- add a suffix: terra43-extras
repoName :: Distro -> Release -> Text
repoName d r = distroRepo d <> relName r

-- | where Fyra Labs' repos are, the same metalink dnf uses. {repo} becomes
-- a 'repoName', with any subrepo suffix
defaultRepoUrl :: Text
defaultRepoUrl = "https://tetsudou.fyralabs.com/metalink?repo={repo}&arch=x86_64"

-- | a repo url template for one release, with {release} filled in and
-- {repo} left for 'Oglyad.Types.repoUrl'
repoUrlFor :: Text -> Release -> Text
repoUrlFor url r = T.replace "{release}" (relName r) url

-- | Fedora's own repos, for the overlap check. rawhide has no updates repo
fedoraReposFor :: Release -> [Text]
fedoraReposFor r
  | isRawhide r = ["https://dl.fedoraproject.org/pub/fedora/linux/development/rawhide/Everything/x86_64/os"]
  | otherwise =
      [ "https://dl.fedoraproject.org/pub/fedora/linux/releases/" <> relName r <> "/Everything/x86_64/os",
        "https://dl.fedoraproject.org/pub/fedora/linux/updates/" <> relName r <> "/Everything/x86_64"
      ]

-- | what --release accepts, before we know rawhide's number
data ReleaseArg = NumberedArg Int | RawhideArg
  deriving stock (Eq, Show)

-- | 43, f43, rawhide or frawhide
parseReleaseArg :: String -> Either String ReleaseArg
parseReleaseArg raw = case dropF raw of
  "rawhide" -> Right RawhideArg
  n
    | not (null n), all isDigit n, Just v <- readMaybe n -> Right (NumberedArg v)
  _ -> Left ("unknown release " <> show raw <> ", expected a number like 43, f43, or rawhide")
  where
    dropF ('f' : rest) = rest
    dropF s = s

-- | every Fedora release Bodhi hasn't archived. that's the current ones, a
-- branched one before its release, and rawhide
bodhiReleasesUrl :: Text
bodhiReleasesUrl = "https://bodhi.fedoraproject.org/releases/?exclude_archived=true&rows_per_page=100"

-- | the maintained Fedora releases in a Bodhi /releases/ response, oldest
-- first with rawhide last. ELN, containers and flatpaks are skipped: only
-- names like F43 count
parseBodhiReleases :: LBS.ByteString -> Either Text [Release]
parseBodhiReleases body = do
  v <- either (Left . ("bad json from Bodhi: " <>) . T.pack) Right (eitherDecode body)
  rels <- case v of
    Object o | Just (Array rs) <- KM.lookup "releases" o -> Right (V.toList rs)
    _ -> Left "no releases in Bodhi's answer"
  let found =
        [ Release (if branch == "rawhide" then "rawhide" else ver) n
        | Object r <- rels,
          Just (String name) <- [KM.lookup "name" r],
          Just ('F', digits) <- [T.uncons name],
          not (T.null digits),
          T.all isDigit digits,
          Just (String ver) <- [KM.lookup "version" r],
          Just n <- [readMaybe (T.unpack ver)],
          Just (String branch) <- [KM.lookup "branch" r]
        ]
  case found of
    [] -> Left "Bodhi listed no Fedora releases"
    _ -> Right (sortOn (\r -> (isRawhide r, relVersion r)) found)

-- | one list of findings out of each release's. a finding that comes up the
-- same way in several releases is shown once, with fReleases naming them.
-- "the same way" ignores the dist tag, so build-lag's
-- "spec is at 1.2-1.fc43" and "spec is at 1.2-1.fc44" merge into
-- "spec is at 1.2-1%{dist}"
mergeFindings :: Distro -> [(Release, [Finding])] -> [Finding]
mergeFindings distro perRelease =
  [ merged k f (r : map snd rest)
  | k <- nubOrd (map fst tagged),
    Just ((f, r) : rest) <- [Map.lookup k grouped]
  ]
  where
    tagged = [(keyOf r f, (f, relName r)) | (r, fs) <- perRelease, f <- fs]
    keyOf r f = (fCheck f, fPackage f, fPath f, fSeverity f, T.replace (releaseDist distro r) "%{dist}" (fMessage f))
    -- members stay in the order the releases were given
    grouped = Map.fromListWith (flip (<>)) [(k, [x]) | (k, x) <- tagged]
    merged (_, _, _, _, generic) f rels = case nub rels of
      [single] -> f {fReleases = [single]}
      many -> f {fReleases = many, fMessage = generic}
