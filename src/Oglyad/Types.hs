module Oglyad.Types
  ( EVR (..),
    UpstreamSource (..),
    CrateField (..),
    Package (..),
    RepoBuild (..),
    Severity (..),
    Finding (..),
    finding,
    Snapshot (..),
    Config (..),
    Check,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value (String))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Oglyad.Version (EVR (..))

-- | which version field of a crate anda reads
data CrateField
  = -- | crates()
    MaxStable
  | -- | crates_max()
    Max
  | -- | crates_newest()
    Newest
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | where a package's upstream version comes from, recovered from update.rhai.
-- constructors mirror the andax helpers in FyraLabs/anda (andax/fns/tsunagu.rs)
data UpstreamSource
  = -- | gh("owner/repo"), latest release
    GitHub Text
  | -- | gh_tag("owner/repo"), newest tag
    GitHubTag Text
  | -- | gitlab([domain,] id), latest release
    GitLab Text Text
  | -- | gitlab_tag([domain,] id)
    GitLabTag Text Text
  | -- | gitea(host, repo) and codeberg(repo)
    Gitea Text Text
  | -- | gitea_tag(host, repo) and codeberg_tag(repo)
    GiteaTag Text Text
  | PyPI Text
  | Crates CrateField Text
  | Npm Text
  | Hackage Text
  | Gems Text
  | Hex Text
  | Cran Text
  | Opam Text
  | -- | namespace, collection
    AnsibleGalaxy Text Text
  | -- | newest tag of any git remote (sourcehut, tangled)
    GitTags Text
  | -- | follows another distro or package (bodhi, madoguchi, nvidia), not upstream
    Derived Text
  | -- | follows a branch head (gh_commit and friends), no version to compare
    CommitTracked
  | -- | raw update.rhai we couldn't make sense of
    Unknown Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | what Terra's published repodata says about a source package
data RepoBuild = RepoBuild
  { rbEVR :: EVR,
    rbBuildTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | one source package, merged from every data source we know about
data Package = Package
  { pkgName :: Text,
    -- | relative to the monorepo, e.g. anda/devs/zed/stable
    pkgPath :: FilePath,
    -- | Terra subrepo from anda.hcl labels, e.g. "extras"
    pkgSubrepo :: Maybe Text,
    -- | what's merged in git
    pkgSpecEVR :: Maybe EVR,
    -- | what's actually published
    pkgRepoBuild :: Maybe RepoBuild,
    pkgLastCommit :: Maybe UTCTime,
    -- | Nothing means no update.rhai
    pkgUpstream :: Maybe UpstreamSource,
    -- | only filled in when upstream lookups ran
    pkgUpstreamVer :: Maybe Text,
    -- | why the upstream lookup failed, if it did
    pkgUpstreamErr :: Maybe Text
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
  { fCheck :: Text,
    fPackage :: Text,
    -- | the package's directory in the monorepo. package names aren't unique
    -- (see the duplicate-name check), so this is what identifies it
    fPath :: Maybe Text,
    fSeverity :: Severity,
    fMessage :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | a finding about a package
finding :: Text -> Package -> Severity -> Text -> Finding
finding check p sev msg =
  Finding
    { fCheck = check,
      fPackage = pkgName p,
      fPath = Just (T.pack (pkgPath p)),
      fSeverity = sev,
      fMessage = msg
    }

data Config = Config
  { -- | local checkout of terrapkg/packages
    cfgMonorepo :: FilePath,
    -- | e.g. https://repos.fyralabs.com/terra44
    cfgTerraRepo :: Text,
    -- | repo base urls to check for overlap
    cfgFedoraRepos :: [Text],
    -- | %{dist} used when evaluating specs, e.g. .fc44
    cfgDist :: Text,
    cfgStaleAfter :: NominalDiffTime,
    -- | whether to hit upstream APIs at all
    cfgUpstream :: Bool,
    -- | reuse cached upstream answers younger than this
    cfgCacheFor :: NominalDiffTime,
    -- | parallel jobs, defaults to the number of cores
    cfgConcurrency :: Int,
    cfgGitHubToken :: Maybe Text
  }
  deriving stock (Show)

-- | everything a check is allowed to look at, already collected.
-- checks never do IO, which is what keeps them trivially testable
data Snapshot = Snapshot
  { -- | keyed by monorepo path, since names can repeat
    snapPackages :: Map FilePath Package,
    -- | source package names present in Fedora
    snapFedora :: Set Text,
    snapNow :: UTCTime,
    snapConfig :: Config
  }

type Check = Snapshot -> Package -> [Finding]
