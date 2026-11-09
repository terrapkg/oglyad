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
import Data.List (maximumBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Oglyad.App (App, HttpError (..), httpGetE)
import Oglyad.Process (run)
import Oglyad.Types (CrateField (..), UpstreamSource (..))
import Oglyad.Version (normalizeVersion, rpmvercmp)

-- | a step into a json document: an object key or an array index
data Step = K Text | I Int

-- | latest upstream version, or a reason we couldn't get it
latestVersion :: UpstreamSource -> App (Either HttpError Text)
latestVersion = \case
  GitHub repo -> json ("https://api.github.com/repos/" <> repo <> "/releases/latest") [K "tag_name"]
  GitHubTag repo -> json ("https://api.github.com/repos/" <> repo <> "/tags?per_page=1") [I 0, K "name"]
  GitLab host i -> json ("https://" <> host <> "/api/v4/projects/" <> i <> "/releases/") [I 0, K "tag_name"]
  GitLabTag host i -> json ("https://" <> host <> "/api/v4/projects/" <> i <> "/repository/tags") [I 0, K "name"]
  Gitea host repo -> json ("https://" <> host <> "/api/v1/repos/" <> repo <> "/releases/latest") [K "tag_name"]
  GiteaTag host repo -> json ("https://" <> host <> "/api/v1/repos/" <> repo <> "/tags") [I 0, K "name"]
  PyPI name -> json ("https://pypi.org/pypi/" <> name <> "/json") [K "info", K "version"]
  Crates field name -> json ("https://crates.io/api/v1/crates/" <> name) [K "crate", K (crateKey field)]
  Npm name -> json ("https://registry.npmjs.org/" <> name <> "/latest") [K "version"]
  Hackage name -> json ("https://hackage.haskell.org/package/" <> name <> "/preferred.json") [K "normal-version", I 0]
  Gems name -> json ("https://rubygems.org/api/v1/versions/" <> name <> "/latest.json") [K "version"]
  Hex name -> json ("https://hex.pm/api/packages/" <> name) [K "latest_stable_version"]
  Cran name -> json ("https://crandb.r-pkg.org/" <> name) [K "Version"]
  Opam name -> opam name
  AnsibleGalaxy ns coll ->
    json
      ("https://galaxy.ansible.com/api/v3/plugin/ansible/content/published/collections/index/" <> ns <> "/" <> coll <> "/versions/?limit=1&ordering=-version")
      [K "data", I 0, K "version"]
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

-- | GET a url and walk a path into the json it returns
json :: Text -> [Step] -> App (Either HttpError Text)
json url path = do
  body <- httpGetE url
  pure $ do
    v <- body >>= either (\e -> Left (HttpFailed ("bad json from " <> url <> ": " <> T.pack e))) Right . eitherDecode
    case walk path v of
      Just (String s) | not (T.null s) -> Right s
      _ -> Left (HttpFailed ("no version at expected spot in " <> url))

walk :: [Step] -> Value -> Maybe Value
walk [] v = Just v
walk (K k : rest) (Object o) = KM.lookup (Key.fromText k) o >>= walk rest
walk (I i : rest) (Array a) = a V.!? i >>= walk rest
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
      _ -> Right (maximumBy (\a b -> rpmvercmp (normalizeVersion a) (normalizeVersion b)) versions)

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

-- | newest tag on a git remote, for forges without a releases api.
-- `git ls-remote` speaks the smart http protocol, so nothing gets cloned
gitTags :: Text -> IO (Either Text Text)
gitTags url = do
  out <- run "git" ["ls-remote", "--tags", "--refs", T.unpack url]
  pure $ case out of
    Left err -> Left ("git ls-remote failed for " <> url <> ": " <> err)
    Right o -> case [t | l <- T.lines o, Just t <- [T.stripPrefix "refs/tags/" (T.takeWhileEnd (/= '\t') l)]] of
      [] -> Left ("no tags at " <> url)
      tags -> Right (maximumBy (\a b -> rpmvercmp (normalizeVersion a) (normalizeVersion b)) tags)
