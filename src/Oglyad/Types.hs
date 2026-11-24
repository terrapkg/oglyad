module Oglyad.Types
  ( EVR (..),
    UpstreamSource (..),
    CrateField (..),
    Package (..),
    RepoBuild (..),
    SubmoduleEvent (..),
    Severity (..),
    Finding (..),
    finding,
    Snapshot (..),
    Config (..),
    RozbirMode (..),
    repoUrl,
    Check,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, (.:), (.=))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Oglyad.Version (EVR (..))

data CrateField
  = -- | crates()
    MaxStable
  | -- | crates_max()
    Max
  | -- | crates_newest()
    Newest
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | mirrors the andax helpers in FyraLabs/anda (andax/fns/tsunagu.rs)
data UpstreamSource
  = GitHub Text
  | GitHubTag Text
  | -- | domain, project id
    GitLab Text Text
  | GitLabTag Text Text
  | -- | host, repo. codeberg() lands here too
    Gitea Text Text
  | GiteaTag Text Text
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
  | GitTags Text
  | -- | follows another distro or package (bodhi, madoguchi, nvidia), not upstream
    Derived Text
  | -- | follows a branch head (gh_commit and friends), so there's no version to compare
    CommitTracked
  | -- | the script only bumps Release or writes files, never the Version: inputs
    NoVersion
  | Unknown Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RepoBuild = RepoBuild
  { rbEVR :: EVR,
    rbBuildTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SubmoduleEvent
  = SubmoduleAdded {sePath :: Text, seUrl :: Text, seCommit :: Text, seGlobal :: Text}
  | SubmoduleDropped {seGlobal :: Text, seCommit :: Text}
  deriving stock (Eq, Show)

instance FromJSON SubmoduleEvent where
  parseJSON = withObject "event" $ \o ->
    o .: "kind" >>= \case
      "submodule-added" -> SubmoduleAdded <$> o .: "path" <*> o .: "url" <*> o .: "commit" <*> o .: "global"
      "submodule-dropped" -> SubmoduleDropped <$> o .: "global" <*> o .: "commit"
      other -> fail ("unknown event " <> T.unpack other)

instance ToJSON SubmoduleEvent where
  toJSON = \case
    SubmoduleAdded {..} ->
      object ["kind" .= ("submodule-added" :: Text), "path" .= sePath, "url" .= seUrl, "commit" .= seCommit, "global" .= seGlobal]
    SubmoduleDropped {..} ->
      object ["kind" .= ("submodule-dropped" :: Text), "global" .= seGlobal, "commit" .= seCommit]

data Package = Package
  { pkgName :: Text,
    -- | relative to the monorepo, e.g. anda/devs/zed/stable
    pkgPath :: FilePath,
    -- | from the anda.hcl labels
    pkgSubrepo :: Maybe Text,
    -- | a `nightly` or `quaterdaily` label: built from a branch head, not releases
    pkgNightly :: Bool,
    pkgUpdBranch :: Bool,
    -- | relative to the tree it was read from
    pkgSpecPath :: Maybe FilePath,
    -- | hash of everything that can change what rpmspec says, keying the anda
    -- run caches
    pkgFingerprint :: Text,
    pkgSpecEVR :: Maybe EVR,
    pkgRepoBuild :: Maybe RepoBuild,
    pkgLastCommit :: Maybe UTCTime,
    -- | Nothing means no update.rhai
    pkgUpstream :: Maybe UpstreamSource,
    pkgRhai :: Maybe Text,
    -- | only filled in with --upstream
    pkgUpstreamVer :: Maybe Text,
    pkgUpstreamErr :: Maybe Text,
    pkgSubmoduleEvents :: [SubmoduleEvent],
    -- | when False, pkgRepoBuild = Nothing means "unknown", not "never published"
    pkgRepoChecked :: Bool
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
    -- | package names aren't unique, the monorepo path is
    fPath :: Maybe Text,
    fSeverity :: Severity,
    fMessage :: Text,
    -- | release names ("43", "rawhide"), empty until the run stamps them
    fReleases :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

finding :: Text -> Package -> Severity -> Text -> Finding
finding check p sev msg =
  Finding
    { fCheck = check,
      fPackage = pkgName p,
      fPath = Just (T.pack (pkgPath p)),
      fSeverity = sev,
      fMessage = msg,
      fReleases = []
    }

data RozbirMode = RozbirOff | RozbirShadow | RozbirFirst
  deriving stock (Eq, Show)

data Config = Config
  { cfgMonorepo :: FilePath,
    cfgRoot :: FilePath,
    -- | Nothing reads the working tree as it is
    cfgSpecRef :: Maybe Text,
    -- | update.rhai scripts see it as labels.branch
    cfgBranch :: Text,
    cfgRolling :: Bool,
    cfgRepoName :: Text,
    -- | a metalink or base url with {repo} where the repo name goes. without
    -- {repo} it's the main repo's url, and subrepos append -extras and so on
    cfgRepoUrl :: Text,
    cfgSubrepos :: Bool,
    cfgBaseRepos :: [Text],
    cfgBaseName :: Text,
    -- | Nothing undefines the macro
    cfgMacros :: [(Text, Maybe Text)],
    cfgCacheTag :: Text,
    cfgStaleAfter :: NominalDiffTime,
    cfgUpstream :: Bool,
    cfgCacheFor :: NominalDiffTime,
    -- | upstream answers fetched since this are reused whatever cfgCacheFor
    -- says, so checking several releases asks each upstream once
    cfgRunStart :: UTCTime,
    cfgConcurrency :: Int,
    cfgGitHubToken :: Maybe Text,
    cfgRozbir :: RozbirMode
  }
  deriving stock (Show)

repoUrl :: Config -> Maybe Text -> Text
repoUrl Config {..} sub
  | "{repo}" `T.isInfixOf` cfgRepoUrl = T.replace "{repo}" (cfgRepoName <> suffix) cfgRepoUrl
  | otherwise = cfgRepoUrl <> suffix
  where
    suffix = maybe "" ("-" <>) sub

-- | everything a check may look at. checks never do IO, which keeps them
-- trivially testable
data Snapshot = Snapshot
  { -- | keyed by monorepo path, since names can repeat
    snapPackages :: Map FilePath Package,
    snapBase :: Set Text,
    snapNow :: UTCTime,
    snapConfig :: Config,
    snapHead :: Maybe Text,
    snapIgnoreLabels :: Map FilePath [Text]
  }

type Check = Snapshot -> Package -> [Finding]
