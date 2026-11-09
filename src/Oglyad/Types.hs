module Oglyad.Types
  ( EVR (..)
  , UpstreamSource (..)
  , CrateField (..)
  , Package (..)
  , RepoBuild (..)
  , Severity (..)
  , Finding (..)
  , Snapshot (..)
  , Config (..)
  , Check
  ) where

import Data.Aeson (FromJSON, ToJSON (..), Value (String))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Oglyad.Version (EVR (..))

-- | which version field of a crate anda reads
data CrateField
  = MaxStable -- ^ crates()
  | Max -- ^ crates_max()
  | Newest -- ^ crates_newest()
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | where a package's upstream version comes from, recovered from update.rhai.
-- constructors mirror the andax helpers in FyraLabs/anda (andax/fns/tsunagu.rs)
data UpstreamSource
  = GitHub Text -- ^ gh("owner/repo"), latest release
  | GitHubTag Text -- ^ gh_tag("owner/repo"), newest tag
  | GitLab Text Text -- ^ gitlab([domain,] id), latest release
  | GitLabTag Text Text -- ^ gitlab_tag([domain,] id)
  | Gitea Text Text -- ^ gitea(host, repo) and codeberg(repo)
  | GiteaTag Text Text -- ^ gitea_tag(host, repo) and codeberg_tag(repo)
  | PyPI Text
  | Crates CrateField Text
  | Npm Text
  | Hackage Text
  | Gems Text
  | Hex Text
  | Cran Text
  | Opam Text
  | AnsibleGalaxy Text Text -- ^ namespace, collection
  | GitTags Text -- ^ newest tag of any git remote (sourcehut, tangled)
  | Derived Text -- ^ follows another distro or package (bodhi, madoguchi, nvidia), not upstream
  | CommitTracked -- ^ follows a branch head (gh_commit and friends), no version to compare
  | Unknown Text -- ^ raw update.rhai we couldn't make sense of
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | what Terra's published repodata says about a source package
data RepoBuild = RepoBuild
  { rbEVR :: EVR
  , rbBuildTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | one source package, merged from every data source we know about
data Package = Package
  { pkgName :: Text
  , pkgPath :: FilePath -- ^ relative to the monorepo, e.g. anda/devs/zed/stable
  , pkgSubrepo :: Maybe Text -- ^ Terra subrepo from anda.hcl labels, e.g. "extras"
  , pkgSpecEVR :: Maybe EVR -- ^ what's merged in git
  , pkgRepoBuild :: Maybe RepoBuild -- ^ what's actually published
  , pkgLastCommit :: Maybe UTCTime
  , pkgUpstream :: Maybe UpstreamSource -- ^ Nothing means no update.rhai
  , pkgUpstreamVer :: Maybe Text -- ^ only filled in when upstream lookups ran
  , pkgUpstreamErr :: Maybe Text -- ^ why the upstream lookup failed, if it did
  }
  deriving stock (Eq, Show)

data Severity = Info | Warn | Error
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Severity where
  toJSON = \case
    Info -> String "info"
    Warn -> String "warn"
    Error -> String "error"

data Finding = Finding
  { fCheck :: Text
  , fPackage :: Text
  , fSeverity :: Severity
  , fMessage :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Config = Config
  { cfgMonorepo :: FilePath -- ^ local checkout of terrapkg/packages
  , cfgTerraRepo :: Text -- ^ e.g. https://repos.fyralabs.com/terra44
  , cfgFedoraRepos :: [Text] -- ^ repo base urls to check for overlap
  , cfgDist :: Text -- ^ %{dist} used when evaluating specs, e.g. .fc44
  , cfgStaleAfter :: NominalDiffTime
  , cfgUpstream :: Bool -- ^ whether to hit upstream APIs at all
  , cfgCacheFor :: NominalDiffTime -- ^ reuse cached upstream answers younger than this
  , cfgConcurrency :: Int -- ^ parallel jobs, defaults to the number of cores
  , cfgGitHubToken :: Maybe Text
  }
  deriving stock (Show)

-- | everything a check is allowed to look at, already collected.
-- checks never do IO, which is what keeps them trivially testable
data Snapshot = Snapshot
  { snapPackages :: Map Text Package
  , snapFedora :: Set Text -- ^ source package names present in Fedora
  , snapNow :: UTCTime
  , snapConfig :: Config
  }

type Check = Snapshot -> Package -> [Finding]
