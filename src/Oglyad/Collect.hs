-- | the imperative shell: gather every data source into one 'Snapshot'
module Oglyad.Collect
  ( buildSnapshot,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Oglyad.App (App, Env (..), forConcurrentlyN, note)
import Oglyad.Source.Cache (CacheEntry (..), UpstreamCache, loadJson, lookupFresh, saveJson)
import Oglyad.Source.Monorepo
import Oglyad.Source.Repodata (fetchRepo)
import Oglyad.Source.Rhai (parseUpdateRhai)
import Oglyad.Source.Upstream (latestVersion)
import Oglyad.Types
import System.Directory (findExecutable, makeAbsolute)
import System.FilePath ((</>))

-- | collect everything. problems along the way become findings instead of
-- crashes, so one broken spec doesn't take down the whole run.
--
-- the monorepo work (rpmspec, git) is all local cpu and the repodata work is
-- all network, so the two halves run at the same time
buildSnapshot :: App (Snapshot, [Finding])
buildSnapshot = do
  cfg@Config {..} <- asks envConfig
  now <- liftIO getCurrentTime
  dirs <- liftIO (findPackageDirs cfgMonorepo)

  -- the main repo, every subrepo some package asked for, and Fedora
  let terraRepos =
        (Nothing, cfgTerraRepo)
          : [(Just s, cfgTerraRepo <> "-" <> s) | s <- nub (mapMaybe pdSubrepo dirs)]
  env <- ask
  ((specErrs, pkgs0), (terra, fedora)) <-
    liftIO $
      concurrently
        (runReaderT (readPackages dirs) env)
        (runReaderT (fetchRepos terraRepos cfgFedoraRepos) env)

  let (repoErrs, repos) = partitionEithers terra
      repoFor p = Map.findWithDefault Map.empty (pkgSubrepo p) (Map.fromList repos)
      pkgs1 = [p {pkgRepoBuild = Map.lookup (pkgName p) (repoFor p)} | p <- pkgs0]
  pkgs <- if cfgUpstream then withUpstreams now pkgs1 else pure pkgs1

  let snap =
        Snapshot
          { snapPackages = Map.fromList [(pkgPath p, p) | p <- pkgs],
            snapFedora = foldMap Map.keysSet fedora,
            snapNow = now,
            snapConfig = cfg
          }
  pure (snap, specErrs <> repoErrs)

-- | every repo at once. Terra failures become findings, Fedora ones just mean
-- no overlap data
fetchRepos ::
  [(Maybe Text, Text)] ->
  [Text] ->
  App ([Either Finding (Maybe Text, Map Text RepoBuild)], [Map Text RepoBuild])
fetchRepos terraRepos fedoraRepos = do
  let jobs = map Left terraRepos <> map Right fedoraRepos
  results <- forConcurrentlyN (length jobs) jobs $ \case
    Left (sub, url) -> Left . either (Left . collectErr url . T.pack) (\b -> Right (sub, b)) <$> fetchRepo url
    Right url -> Right . either (const Map.empty) id <$> fetchRepo url
  pure (partitionEithers results)

-- | evaluate every spec, reusing cached evaluations for unchanged packages
readPackages :: [PkgDir] -> App ([Finding], [Package])
readPackages dirs = do
  Config {..} <- asks envConfig
  root <- liftIO (makeAbsolute cfgMonorepo)
  (commits, (macros, cache)) <-
    liftIO $
      concurrently
        (commitTimes cfgMonorepo (map pdPath dirs))
        ((,) <$> macroFingerprint <*> (fromMaybe Map.empty <$> loadJson "specs.json"))
  keyed <- forConcurrentlyN cfgConcurrency dirs $ \d -> liftIO $ do
    key <- specFingerprint macros cfgDist root (pdPath d)
    pure (key, d)
  let misses = length [() | (k, _) <- keyed, isNothing (Map.lookup k (cache :: Map Text (Either Text SpecInfo)))]
  note ("evaluating " <> show (length dirs) <> " specs, " <> show (length dirs - misses) <> " unchanged since last run")
  -- without rpmspec, cached evaluations still work. everything else is
  -- skipped and reported once, rather than as one error per package
  hasRpmspec <- liftIO (isJust <$> findExecutable "rpmspec")
  results <- forConcurrentlyN cfgConcurrency keyed $ \(key, d) -> liftIO $ do
    case (Map.lookup key cache, pdSpec d) of
      (Just cached, _) -> pure (Evaluated key d cached)
      (Nothing, Just spec)
        | hasRpmspec -> Evaluated key d <$> querySpec cfgDist (root </> spec)
        | otherwise -> pure Skipped
      (Nothing, Nothing) -> pure (NoSpec d)
  -- keep only entries for packages that still exist, so the file can't grow forever
  liftIO (saveJson "specs.json" (Map.fromList [(k, i) | Evaluated k _ i <- results]))
  let skipped = length [() | Skipped <- results]
      missing =
        [ collectErr "rpmspec" ("rpmspec not found, install rpm-build. skipped " <> T.pack (show skipped) <> " specs with no cached result")
        | skipped > 0
        ]
      (errs, pkgs) =
        partitionEithers $
          [toPackage commits d (Just i) | Evaluated _ d i <- results]
            <> [toPackage commits d Nothing | NoSpec d <- results]
  pure (missing <> errs, pkgs)

-- | what happened to one package dir's spec
data SpecResult
  = Evaluated Text PkgDir (Either Text SpecInfo)
  | NoSpec PkgDir
  | Skipped

-- | last commit per package dir, reused as long as HEAD hasn't moved.
-- uncommitted changes can't change any "last commit", so HEAD is the whole key
commitTimes :: FilePath -> [FilePath] -> IO (Map FilePath UTCTime)
commitTimes root dirs = do
  hd <- headCommit root
  cached <- loadJson "commits.json"
  case (hd, cached) of
    (Just h, Just (h', m)) | h == h', all (`Map.member` m) dirs -> pure m
    _ -> do
      m <- lastCommits root dirs
      mapM_ (\h -> saveJson "commits.json" (h, m)) hd
      pure m

toPackage :: Map FilePath UTCTime -> PkgDir -> Maybe (Either Text SpecInfo) -> Either Finding Package
toPackage commits PkgDir {..} = \case
  Nothing -> Left (pathErr "no spec file found")
  Just (Left err) -> Left (pathErr ("rpmspec failed: " <> firstLine err))
  Just (Right SpecInfo {..}) ->
    Right
      Package
        { pkgName = siName,
          pkgPath = pdPath,
          pkgSubrepo = pdSubrepo,
          pkgSpecEVR = Just siEVR,
          pkgRepoBuild = Nothing,
          pkgLastCommit = Map.lookup pdPath commits,
          pkgUpstream = parseUpdateRhai <$> pdUpdateRhai,
          pkgUpstreamVer = Nothing,
          pkgUpstreamErr = Nothing
        }
  where
    firstLine t = let l = T.takeWhile (/= '\n') t in if T.null l then "(no output)" else l
    pathErr msg = (collectErr (T.pack pdPath) msg) {fPath = Just (T.pack pdPath)}

-- | fill in upstream versions, from the cache when it's fresh enough
withUpstreams :: UTCTime -> [Package] -> App [Package]
withUpstreams now pkgs = do
  Config {..} <- asks envConfig
  when (isNothing cfgGitHubToken) $
    note "no GITHUB_TOKEN (or `gh auth token`), GitHub lookups will hit the 60/hour limit fast"
  cache <- liftIO (fromMaybe Map.empty <$> loadJson "upstream.json") :: App UpstreamCache
  let wanted = [p | p <- pkgs, Just src <- [pkgUpstream p], lookable src]
      fresh p = pkgUpstream p >>= \src -> lookupFresh cfgCacheFor now src cache
  note ("querying upstreams for " <> show (length wanted) <> " packages, " <> show (length (filter (isJust . fresh) wanted)) <> " cached")
  results <- forConcurrentlyN cfgConcurrency wanted $ \p -> case (fresh p, pkgUpstream p) of
    (Just e, _) -> pure (pkgPath p, ceResult e)
    (Nothing, Just src) -> (pkgPath p,) <$> latestVersion src
    (Nothing, Nothing) -> pure (pkgPath p, Left "no source")
  -- keyed by path, not name: two dirs can share a package name
  let byPath = Map.fromList results :: Map FilePath (Either Text Text)
      -- only cache successes. errors are often transient (rate limits, timeouts)
      new =
        Map.fromList
          [ (T.pack (show src), CacheEntry now (Right v))
          | p <- wanted,
            isNothing (fresh p),
            Just src <- [pkgUpstream p],
            Just (Right v) <- [Map.lookup (pkgPath p) byPath]
          ]
  liftIO (saveJson "upstream.json" (new <> cache))
  pure
    [ case Map.lookup (pkgPath p) byPath of
        Just (Right v) -> p {pkgUpstreamVer = Just v}
        Just (Left e) -> p {pkgUpstreamErr = Just e}
        Nothing -> p
    | p <- pkgs
    ]
  where
    lookable = \case
      CommitTracked -> False
      Derived _ -> False
      Unknown _ -> False
      _ -> True

collectErr :: Text -> Text -> Finding
collectErr what msg = Finding {fCheck = "collect", fPackage = what, fPath = Nothing, fSeverity = Error, fMessage = msg}
