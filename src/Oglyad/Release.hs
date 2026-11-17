-- | which releases to check, and everything that follows from one and the
-- distro being checked: the repo, the %{dist} tag, the monorepo branch, the
-- rpm macros and the base distro's repos. also merges findings from several
-- releases into one report
module Oglyad.Release
  ( Base (..),
    baseName,
    baseRepoOwners,
    Distro (..),
    terra,
    terraEl,
    ultramarine,
    tuatara,
    distros,
    lookupDistro,
    remoteRepoName,
    Release (..),
    isRolling,
    tumbleweed,
    el,
    elReleasesFrom,
    releaseLabel,
    releaseDist,
    releaseBranch,
    releaseMacros,
    releaseSubrepos,
    releaseCacheTag,
    repoName,
    defaultRepoUrl,
    repoUrlFor,
    baseReposFor,
    parseReleaseArg,
    ReleaseArg (..),
    releaseArgBase,
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

data Base = Fedora | OpenSUSE | EL
  deriving stock (Eq, Show)

baseName :: Base -> Text
baseName = \case
  Fedora -> "Fedora"
  OpenSUSE -> "openSUSE"
  EL -> "EL"

-- | who ships the packages in 'baseReposFor', as the overlap check names them
baseRepoOwners :: Base -> Text
baseRepoOwners = \case
  EL -> "AlmaLinux or EPEL"
  b -> baseName b

-- | an anda monorepo oglyad knows how to check, and how it spells things per
-- release
data Distro = Distro
  { -- | what --distro takes, e.g. "terra"
    distroName :: Text,
    -- | what it builds for. releases default to the first one's
    distroBases :: [Base],
    -- | the directory under the monorepo root holding every package, e.g.
    -- "anda" for terrapkg/packages
    distroRoot :: FilePath,
    distroMarker :: FilePath,
    -- | the monorepo's GitHub repo name, which tells distros with the same
    -- marker apart
    distroUpstream :: Text,
    -- | the repo's name, with the release name appended: terra for terra43
    distroRepo :: Text,
    -- | Fedora release branch prefix, f for f43
    distroBranch :: Text,
    -- | Fedora %{dist} prefix, .fc for .fc43
    distroDist :: Text,
    distroRepoUrl :: Text,
    distroSubrepos :: Bool,
    distroSkippedChecks :: [Text]
  }
  deriving stock (Eq, Show)

-- | terrapkg/packages. its el10 branch is still around until Terra EL
-- finishes moving to 'terraEl'
terra :: Distro
terra = Distro "terra" [Fedora, EL] "anda" "anda" "packages" "terra" "f" ".fc" defaultRepoUrl True []

-- | terrapkg/packages-el. builds land in terrael10, with no subrepos
terraEl :: Distro
terraEl = Distro "terra-el" [EL] "anda" "anda" "packages-el" "terra" "" "" defaultRepoUrl False []

-- | Ultramarine-Linux/packages. builds land in um43, umrawhide, with
-- .um43 and .umrawhide dist tags
ultramarine :: Distro
ultramarine = Distro "ultramarine" [Fedora] "ultramarine" "ultramarine" "packages" "um" "um" ".um" defaultRepoUrl True ["no-autoupdate"]

tuatara :: Distro
tuatara = Distro "tuatara" [OpenSUSE] "" "tuatara" "tuatara" "tuatara-" "" "" "https://repos.fyralabs.com/{repo}" False []

distros :: [Distro]
distros = [terra, terraEl, ultramarine, tuatara]

lookupDistro :: String -> Either String Distro
lookupDistro raw = case [d | d <- distros, distroName d == T.pack raw] of
  d : _ -> Right d
  [] -> Left ("unknown distro " <> show raw <> ", expected one of: " <> T.unpack (T.intercalate ", " (map distroName distros)))

-- | the repo name at the end of a git remote url: packages-el for
-- git@github.com:terrapkg/packages-el.git
remoteRepoName :: Text -> Maybe Text
remoteRepoName url = case T.split (`elem` ['/', ':']) (T.dropWhileEnd (== '/') (T.strip url)) of
  parts@(_ : _ : _) | name <- dropGit (last parts), not (T.null name) -> Just name
  _ -> Nothing
  where
    dropGit n = maybe n id (T.stripSuffix ".git" n)

-- | a base distro release a monorepo builds for. rawhide keeps its number
-- too, since %fedora still needs one when evaluating specs
data Release = Release
  { relBase :: Base,
    -- | "43", "rawhide", "tumbleweed" or "el10"
    relName :: Text,
    -- | the number %fedora, %suse_version or %rhel expands to
    relVersion :: Int
  }
  deriving stock (Eq, Show)

isRolling :: Release -> Bool
isRolling r = relName r `elem` ["rawhide", "tumbleweed"]

tumbleweed :: Release
tumbleweed = Release OpenSUSE "tumbleweed" 1699

el :: Int -> Release
el n = Release EL ("el" <> T.pack (show n)) n

-- | the EL releases a remote has branches for, oldest first: el10 for
-- origin/el10
elReleasesFrom :: [Text] -> [Release]
elReleasesFrom branches =
  sortOn relVersion
    [ el n
    | b <- nub branches,
      Just digits <- [T.stripPrefix "el" b],
      not (T.null digits),
      T.all isDigit digits,
      Just n <- [readMaybe (T.unpack digits)]
    ]

-- | how releases are spelled in output: f43, rawhide, tumbleweed, el10
releaseLabel :: Release -> Text
releaseLabel r
  | relBase r == Fedora && not (isRolling r) = "f" <> relName r
  | otherwise = relName r

-- | rawhide builds use .fcrawhide, not .fc46. terrapkg/packages' bootstrap
-- workflow rewrites macros.dist to do it, and Ultramarine does the same
releaseDist :: Distro -> Release -> Text
releaseDist d r = case relBase r of
  Fedora -> distroDist d <> relName r
  OpenSUSE -> ""
  EL -> "." <> relName r

-- | %fedora comes from the host otherwise, so -r 43 on an F44 box would
-- take the wrong side of every %if 0%{?fedora} >= 44
releaseMacros :: Distro -> Release -> [(Text, Maybe Text)]
releaseMacros d r = case relBase r of
  Fedora -> [("dist", Just (releaseDist d r)), ("fedora", Just version)]
  OpenSUSE ->
    [ ("dist", Nothing),
      ("fedora", Nothing),
      ("suse_version", Just version),
      ("is_opensuse", Just "1")
    ]
  EL ->
    [ ("dist", Just (releaseDist d r)),
      ("fedora", Nothing),
      ("rhel", Just version),
      (relName r, Just "1"),
      ("epel", Just version)
    ]
  where
    version = T.pack (show (relVersion r))

releaseCacheTag :: Distro -> Release -> Text
releaseCacheTag d r = "-" <> distroName d <> "-" <> relName r

-- | the monorepo branch for a release: f43, frawhide, um43, tumbleweed, el10
releaseBranch :: Distro -> Release -> Text
releaseBranch d r = case relBase r of
  Fedora -> distroBranch d <> relName r
  _ -> relName r

-- | Terra's EL builds only ever upload to terrael10, whatever subrepo label
-- a package has
releaseSubrepos :: Distro -> Release -> Bool
releaseSubrepos d r = distroSubrepos d && relBase r == Fedora

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

-- | the base distro's own repos, for the overlap check. rawhide has no
-- updates repo. EL builds run on AlmaLinux with EPEL enabled, so both count
baseReposFor :: Release -> [Text]
baseReposFor r = case relBase r of
  OpenSUSE ->
    [ "https://download.opensuse.org/tumbleweed/repo/oss",
      "https://download.opensuse.org/update/tumbleweed"
    ]
  EL ->
    [ "https://repo.almalinux.org/almalinux/" <> major <> "/" <> repo <> "/x86_64/os"
    | repo <- ["BaseOS", "AppStream", "CRB", "extras"]
    ]
      <> ["https://dl.fedoraproject.org/pub/epel/" <> major <> "/Everything/x86_64"]
  Fedora -> fedoraRepos
  where
    major = T.pack (show (relVersion r))
    fedoraRepos      | isRolling r = ["https://dl.fedoraproject.org/pub/fedora/linux/development/rawhide/Everything/x86_64/os"]
      | otherwise =
          [ "https://dl.fedoraproject.org/pub/fedora/linux/releases/" <> relName r <> "/Everything/x86_64/os",
            "https://dl.fedoraproject.org/pub/fedora/linux/updates/" <> relName r <> "/Everything/x86_64"
          ]

-- | what --release accepts, before we know the distro or rawhide's number
data ReleaseArg = NumberedArg Int | RawhideArg | TumbleweedArg | ElArg Int
  deriving stock (Eq, Show)

releaseArgBase :: ReleaseArg -> Base
releaseArgBase = \case
  NumberedArg _ -> Fedora
  RawhideArg -> Fedora
  TumbleweedArg -> OpenSUSE
  ElArg _ -> EL

-- | 43, f43, rawhide, frawhide, tumbleweed, tw or el10
parseReleaseArg :: String -> Either String ReleaseArg
parseReleaseArg raw = case raw of
  "tumbleweed" -> Right TumbleweedArg
  "tw" -> Right TumbleweedArg
  'e' : 'l' : n | Just v <- number n -> Right (ElArg v)
  _ -> case dropF raw of
    "rawhide" -> Right RawhideArg
    n | Just v <- number n -> Right (NumberedArg v)
    _ -> Left ("unknown release " <> show raw <> ", expected a number like 43, f43, rawhide, tumbleweed, or el10")
  where
    dropF ('f' : rest) = rest
    dropF s = s
    number n
      | not (null n), all isDigit n = readMaybe n
      | otherwise = Nothing

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
        [ Release Fedora (if branch == "rawhide" then "rawhide" else ver) n
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
    _ -> Right (sortOn (\r -> (isRolling r, relVersion r)) found)

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
    keyOf r f = (fCheck f, fPackage f, fPath f, fSeverity f, distless r (fMessage f))
    distless r msg = case releaseDist distro r of
      "" -> msg
      dist -> T.replace dist "%{dist}" msg
    -- members stay in the order the releases were given
    grouped = Map.fromListWith (flip (<>)) [(k, [x]) | (k, x) <- tagged]
    merged (_, _, _, _, generic) f rels = case nub rels of
      [single] -> f {fReleases = [single]}
      many -> f {fReleases = many, fMessage = generic}
