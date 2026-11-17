-- | asks upstreams what their latest version is, using the same endpoints and
-- json fields as anda's andax helpers (andax/fns/tsunagu.rs in FyraLabs/anda)
-- so oglyad sees exactly what `anda update` would see
module Oglyad.Source.Upstream
  ( latestVersion,
    isSemver,
  )
where

import Control.Monad.Reader (liftIO)
import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isDigit)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Oglyad.App (App, HttpError (..), httpGetE)
import Oglyad.Process (run)
import Oglyad.Types (CrateField (..), UpstreamSource (..))
import Oglyad.Version (normalizeVersion, rpmvercmp)

data JsonStep = Field Text | Index Int

latestVersion :: UpstreamSource -> App (Either HttpError Text)
latestVersion = \case
  GitHub repo -> jsonAt ("https://api.github.com/repos/" <> repo <> "/releases/latest") [Field "tag_name"]
  GitHubTag repo -> jsonAt ("https://api.github.com/repos/" <> repo <> "/tags?per_page=1") [Index 0, Field "name"]
  GitLab host i -> jsonAt ("https://" <> host <> "/api/v4/projects/" <> i <> "/releases/") [Index 0, Field "tag_name"]
  GitLabTag host i -> jsonAt ("https://" <> host <> "/api/v4/projects/" <> i <> "/repository/tags") [Index 0, Field "name"]
  Gitea host repo -> jsonAt ("https://" <> host <> "/api/v1/repos/" <> repo <> "/releases/latest") [Field "tag_name"]
  GiteaTag host repo -> jsonAt ("https://" <> host <> "/api/v1/repos/" <> repo <> "/tags") [Index 0, Field "name"]
  PyPI name -> jsonAt ("https://pypi.org/pypi/" <> name <> "/json") [Field "info", Field "version"]
  Crates field name -> jsonAt ("https://crates.io/api/v1/crates/" <> name) [Field "crate", Field (crateKey field)]
  Npm name -> jsonAt ("https://registry.npmjs.org/" <> name <> "/latest") [Field "version"]
  Hackage name -> jsonAt ("https://hackage.haskell.org/package/" <> name <> "/preferred.json") [Field "normal-version", Index 0]
  Gems name -> jsonAt ("https://rubygems.org/api/v1/versions/" <> name <> "/latest.json") [Field "version"]
  Hex name -> jsonAt ("https://hex.pm/api/packages/" <> name) [Field "latest_stable_version"]
  Cran name -> jsonAt ("https://crandb.r-pkg.org/" <> name) [Field "Version"]
  Opam name -> opam name
  AnsibleGalaxy ns coll ->
    jsonAt
      ("https://galaxy.ansible.com/api/v3/plugin/ansible/content/published/collections/index/" <> ns <> "/" <> coll <> "/versions/?limit=1&ordering=-version")
      [Field "data", Index 0, Field "version"]
  GitTags url -> first HttpFailed <$> liftIO (gitTags url)
  Derived how -> pure (Left (HttpFailed ("follows " <> how <> ", not an upstream release")))
  CommitTracked -> pure (Left (HttpFailed "tracks a commit, not a version"))
  NoVersion -> pure (Left (HttpFailed "doesn't set a version"))
  Unknown _ -> pure (Left (HttpFailed "unparsed update.rhai"))
  where
    crateKey = \case
      MaxStable -> "max_stable_version"
      Max -> "max_version"
      Newest -> "newest_version"

jsonAt :: Text -> [JsonStep] -> App (Either HttpError Text)
jsonAt url path = do
  body <- httpGetE url
  pure $ do
    v <- body >>= either (\e -> Left (HttpFailed ("bad json from " <> url <> ": " <> T.pack e))) Right . eitherDecode
    case walk path v of
      Just (String s) | not (T.null s) -> Right s
      _ -> Left (HttpFailed ("no version at expected spot in " <> url))

walk :: [JsonStep] -> Value -> Maybe Value
walk [] v = Just v
walk (Field k : rest) (Object o) = KM.lookup (Key.fromText k) o >>= walk rest
walk (Index i : rest) (Array a) = a V.!? i >>= walk rest
walk _ _ = Nothing

-- | opam has no "latest" endpoint. anda lists the package's directory in
-- ocaml/opam-repository and picks the highest `name.version` entry that
-- parses as semver, which skips old schemes like Jane Street's 113.24.00
opam :: Text -> App (Either HttpError Text)
opam name = do
  let url = "https://api.github.com/repos/ocaml/opam-repository/contents/packages/" <> name
  body <- httpGetE url
  pure $ do
    v <- body >>= either (Left . HttpFailed . T.pack) Right . eitherDecode
    let versions = case v of
          Array entries ->
            [ ver
            | Object o <- V.toList entries,
              Just (String n) <- [KM.lookup "name" o],
              Just ver <- [T.stripPrefix (name <> ".") n],
              isSemver ver
            ]
          _ -> []
    case versions of
      [] -> Left (HttpFailed ("no versions listed at " <> url))
      _ -> Right (newest versions)

-- | does this parse with the semver crate after anda's opam cleanup (leading
-- v's dropped, '~' read as '-')? MAJOR.MINOR.PATCH with no leading zeros,
-- then an optional -prerelease and +build
isSemver :: Text -> Bool
isSemver raw =
  let v = T.replace "~" "-" (T.dropWhile (== 'v') raw)
      (noBuild, build) = T.breakOn "+" v
      (core, pre) = T.breakOn "-" noBuild
      numeric p = not (T.null p) && T.all isDigit p && (p == "0" || T.head p /= '0')
      ident p = not (T.null p) && T.all (\c -> isAlphaNum c || c == '-') p
      preIdent p = ident p && (not (T.all isDigit p) || numeric p)
      idents f t = T.null t || all f (T.splitOn "." (T.drop 1 t))
   in case T.splitOn "." core of
        [a, b, c] -> all numeric [a, b, c] && idents preIdent pre && idents ident build
        _ -> False

-- | `git ls-remote` speaks the smart http protocol, so nothing gets cloned
gitTags :: Text -> IO (Either Text Text)
gitTags url = do
  out <- run "git" ["ls-remote", "--tags", "--refs", T.unpack url]
  pure $ case out of
    Left err -> Left ("git ls-remote failed for " <> url <> ": " <> err)
    Right refs -> case tagNames refs of
      [] -> Left ("no tags at " <> url)
      tags -> Right (newest tags)

tagNames :: Text -> [Text]
tagNames lsRemote = [tag | l <- T.lines lsRemote, Just tag <- [T.stripPrefix "refs/tags/" (T.takeWhileEnd (/= '\t') l)]]

newest :: [Text] -> Text
newest = maximumBy (rpmvercmp `on` normalizeVersion)
