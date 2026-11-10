-- | the imperative shell: gather every data source into one 'Snapshot'
module Oglyad.Collect
  ( buildSnapshot,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Monad (forM, when)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Data.Either (partitionEithers)
import Data.List (nub, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Oglyad.App (App, Env (..), HttpError (..), forConcurrentlyN, note)
import Oglyad.Source.Anda (projectNames, updateProject, withScratchCopy)
import Oglyad.Source.Cache (CacheEntry (..), UpstreamCache, loadJson, lookupFresh, saveJson)
import Oglyad.Source.Monorepo
import Oglyad.Source.Repodata (fetchRepo)
import Oglyad.Source.Rhai (parseUpdateRhaiWith, tokenize)
import Oglyad.Source.RhaiEval (Simulated (..), simulateVersion)
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
  -- specs come from a release branch extracted into the cache, or straight
  -- from the working tree with --worktree
  tree <- liftIO $ case cfgSpecRef of
    Nothing -> Right . (cfgMonorepo,"HEAD",) <$> headCommit cfgMonorepo
    Just ref -> fmap (\(dir, sha) -> (dir, sha, Just sha)) <$> checkoutRef cfgMonorepo ref
  let (root, logRef, hd, treeErrs) = case tree of
        Right (r, l, h) -> (Just r, l, h, [])
        Left err -> (Nothing, "HEAD", Nothing, [collectErr (maybe "monorepo" id cfgSpecRef) err])
  (dirErrs, dirs) <- partitionEithers <$> liftIO (maybe (pure []) (`findPackageDirs` cfgRoot) root)

  -- the main repo, every subrepo some package asked for, and Fedora
  let terraRepos = [(s, repoUrl cfg s) | s <- Nothing : nub (map Just (mapMaybe pdSubrepo dirs))]
  env <- ask
  ((specErrs, pkgs0), (terra, fedora)) <-
    liftIO $
      concurrently
        (runReaderT (readPackages (maybe cfgMonorepo id root) logRef hd dirs) env)
        (runReaderT (fetchRepos terraRepos cfgFedoraRepos) env)

  let (repoErrs, repos) = partitionEithers terra
      fetched = Map.fromList repos
      pkgs1 =
        [ p
            { pkgRepoBuild = Map.lookup (pkgSubrepo p) fetched >>= Map.lookup (pkgName p),
              pkgRepoChecked = Map.member (pkgSubrepo p) fetched
            }
        | p <- pkgs0
        ]
  (upErrs, pkgs2) <- if cfgUpstream then withUpstreams now pkgs1 else pure ([], pkgs1)
  (andaErrs, pkgs) <- case root of
    Just r | cfgUpstream -> withAnda r now pkgs2
    _ -> pure ([], pkgs2)

  let snap =
        Snapshot
          { snapPackages = Map.fromList [(pkgPath p, p) | p <- pkgs],
            snapFedora = foldMap Map.keysSet fedora,
            snapNow = now,
            snapConfig = cfg,
            snapHead = hd,
            snapIgnoreLabels = Map.fromList [(pdPath d, pdIgnores d) | d <- dirs, not (null (pdIgnores d))]
          }
      unreadable =
        [ (collectErr (T.pack path) ("can't read package dir: " <> err)) {fPath = Just (T.pack path)}
        | (path, err) <- dirErrs
        ]
  pure (snap, treeErrs <> unreadable <> specErrs <> repoErrs <> upErrs <> andaErrs)

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

-- | evaluate every spec under root, reusing cached evaluations for unchanged
-- packages. logRef and hd say which commit's history to read commit times from
readPackages :: FilePath -> Text -> Maybe Text -> [PkgDir] -> App ([Finding], [Package])
readPackages root0 logRef hd dirs = do
  Config {..} <- asks envConfig
  root <- liftIO (makeAbsolute root0)
  -- one cache file per dist, so cron jobs checking different releases don't
  -- keep evicting each other's entries
  let cacheFile = "specs" <> T.unpack cfgDist <> ".json"
  (commits, (macros, cache)) <-
    liftIO $
      concurrently
        (commitTimes cfgMonorepo cfgRoot logRef hd cfgDist (map pdPath dirs))
        ((,) <$> macroFingerprint <*> (fromMaybe Map.empty <$> loadJson cacheFile))
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
        | hasRpmspec -> Evaluated key d <$> querySpec cfgRelease cfgDist (root </> spec)
        | otherwise -> pure Skipped
      (Nothing, Nothing) -> pure (NoSpec d)
  -- keep only entries for packages that still exist, so the file can't grow forever
  liftIO (saveJson cacheFile (Map.fromList [(k, i) | Evaluated k _ i <- results]))
  let skipped = length [() | Skipped <- results]
      missing =
        [ collectErr "rpmspec" ("rpmspec not found, install rpm-build. skipped " <> T.pack (show skipped) <> " specs with no cached result")
        | skipped > 0
        ]
      (errs, pkgs) =
        partitionEithers $
          [toPackage commits k d (Just i) | Evaluated k d i <- results]
            <> [toPackage commits "" d Nothing | NoSpec d <- results]
  pure (missing <> errs, pkgs)

-- | what happened to one package dir's spec
data SpecResult
  = Evaluated Text PkgDir (Either Text SpecInfo)
  | NoSpec PkgDir
  | Skipped

-- | last commit per package dir, reused as long as the commit hasn't moved.
-- uncommitted changes can't change any "last commit", so the commit is the
-- whole key. one cache file per dist, since each release reads its own branch
commitTimes :: FilePath -> FilePath -> Text -> Maybe Text -> Text -> [FilePath] -> IO (Map FilePath UTCTime)
commitTimes mono pkgRoot ref hd dist dirs = do
  let file = "commits" <> T.unpack dist <> ".json"
  cached <- loadJson file
  case (hd, cached) of
    (Just h, Just (h', m)) | h == h', all (`Map.member` m) dirs -> pure m
    _ -> do
      m <- lastCommits mono pkgRoot ref dirs
      mapM_ (\h -> saveJson file (h, m)) hd
      pure m

toPackage :: Map FilePath UTCTime -> Text -> PkgDir -> Maybe (Either Text SpecInfo) -> Either Finding Package
toPackage commits key PkgDir {..} = \case
  Nothing -> Left (pathErr "no spec file found")
  Just (Left err) -> Left (pathErr ("rpmspec failed: " <> firstLine err))
  Just (Right SpecInfo {..}) ->
    Right
      Package
        { pkgName = siName,
          pkgPath = pdPath,
          pkgSubrepo = pdSubrepo,
          pkgNightly = pdNightly,
          pkgSpecPath = pdSpec,
          pkgFingerprint = key,
          pkgSpecEVR = Just siEVR,
          pkgRepoBuild = Nothing,
          pkgLastCommit = Map.lookup pdPath commits,
          -- an update.rhai with nothing but comments in it updates nothing
          pkgUpstream = parseUpdateRhaiWith pdVersionMacros <$> (pdUpdateRhai >>= nonEmpty),
          pkgRhai = pdUpdateRhai,
          pkgUpstreamVer = Nothing,
          pkgUpstreamErr = Nothing,
          pkgRepoChecked = False
        }
  where
    firstLine t = let l = T.takeWhile (/= '\n') t in if T.null l then "(no output)" else l
    nonEmpty script = case tokenize script of
      Just [] -> Nothing
      _ -> Just script
    pathErr msg = (collectErr (T.pack pdPath) msg) {fPath = Just (T.pack pdPath)}

-- | fill in upstream versions, from the cache when it's fresh enough.
--
-- crates.io allows one request a second, so its lookups run in their own
-- serial queue next to everything else. inside the shared job pool, a cluster
-- of crates packages would hold every slot while waiting their turn
withUpstreams :: UTCTime -> [Package] -> App ([Finding], [Package])
withUpstreams now pkgs = do
  Config {..} <- asks envConfig
  when (isNothing cfgGitHubToken) $
    note "no GITHUB_TOKEN (or `gh auth token`), GitHub lookups will hit the 60/hour limit fast"
  cache <- liftIO (fromMaybe Map.empty <$> loadJson "upstream.json") :: App UpstreamCache
  let wanted = [p | p <- pkgs, Just src <- [pkgUpstream p], lookable src]
      -- anything fetched since this run started is fresh, so checking several
      -- releases asks each upstream once even with --cache-hours 0
      maxAge = max cfgCacheFor (diffUTCTime now cfgRunStart)
      fresh p = pkgUpstream p >>= \src -> lookupFresh maxAge now src cache
      (crates, others) = partition (isCrates . pkgUpstream) wanted
      lookup1 p = case (fresh p, pkgUpstream p) of
        (Just e, _) -> pure (pkgPath p, either (Left . HttpFailed) Right (ceResult e))
        (Nothing, Just src) -> (pkgPath p,) <$> latestVersion src
        (Nothing, Nothing) -> pure (pkgPath p, Left (HttpFailed "no source"))
  note ("querying upstreams for " <> show (length wanted) <> " packages, " <> show (length (filter (isJust . fresh) wanted)) <> " cached")
  env <- ask
  (r1, r2) <-
    liftIO $
      concurrently
        (runReaderT (forConcurrentlyN cfgConcurrency others lookup1) env)
        (runReaderT (forM crates lookup1) env)
  -- keyed by path, not name: two dirs can share a package name
  let byPath = Map.fromList (r1 <> r2)
      -- only cache successes. errors are often transient (rate limits, timeouts)
      new =
        Map.fromList
          [ (T.pack (show src), CacheEntry now (Right v))
          | p <- wanted,
            isNothing (fresh p),
            Just src <- [pkgUpstream p],
            Just (Right v) <- [Map.lookup (pkgPath p) byPath]
          ]
      -- forget answers nobody has asked for in a month
      recent = Map.filter (\e -> diffUTCTime now (ceFetched e) < 30 * nominalDay) cache
      limited = [why | Left (RateLimited why) <- Map.elems byPath]
      rateErr = case limited of
        [] -> []
        why : _ ->
          [ (collectErr "upstream" (why <> ", skipped " <> T.pack (show (length limited)) <> " lookups. set GITHUB_TOKEN, or try again later"))
              { fSeverity = Warn
              }
          ]
  liftIO (saveJson "upstream.json" (new <> recent))
  pure
    ( rateErr,
      [ case Map.lookup (pkgPath p) byPath of
          Just (Right v) -> p {pkgUpstreamVer = replay p v}
          Just (Left (HttpFailed e)) -> p {pkgUpstreamErr = Just e}
          -- rate limits are reported once above, not as a broken upstream
          Just (Left (RateLimited _)) -> p
          Nothing -> p
      | p <- pkgs
      ]
    )
  where
    -- run the script's guards and edits over the raw value, so a tag the
    -- script would skip (or crop) doesn't show up as drift
    replay p v = case (pkgUpstream p, pkgRhai p) of
      (Just src, Just script) -> case simulateVersion src v script of
        Sets v' -> Just v'
        NoUpdate -> Nothing
        CantTell -> Just v
      _ -> Just v
    lookable = \case
      CommitTracked -> False
      NoVersion -> False
      Derived _ -> False
      Unknown _ -> False
      _ -> True
    isCrates = \case
      Just (Crates _ _) -> True
      _ -> False

-- | run the real anda for scripts oglyad couldn't follow, on a throwaway
-- copy of the tree, and read the Version it leaves in each spec. results are
-- cached like upstream answers, keyed by the package's fingerprint
withAnda :: FilePath -> UTCTime -> [Package] -> App ([Finding], [Package])
withAnda root now pkgs = do
  Config {..} <- asks envConfig
  let wanted = [p | p@Package {pkgUpstream = Just (Unknown _), pkgSpecPath = Just _} <- pkgs]
      cacheFile = "anda" <> T.unpack cfgDist <> ".json"
      maxAge = max cfgCacheFor (diffUTCTime now cfgRunStart)
  cache <- liftIO (fromMaybe Map.empty <$> loadJson cacheFile) :: App (Map Text CacheEntry)
  let fresh p = case Map.lookup (pkgFingerprint p) cache of
        Just e | diffUTCTime now (ceFetched e) <= maxAge -> Just e
        _ -> Nothing
      misses = [p | p <- wanted, isNothing (fresh p)]
  hasAnda <- liftIO (isJust <$> findExecutable "anda")
  (problems, ran) <-
    if
      | null misses -> pure ([], Map.empty)
      | not hasAnda -> do
          note ("anda not found, so " <> show (length misses) <> " update.rhai scripts oglyad can't follow are skipped")
          pure ([], Map.empty)
      | otherwise -> do
          note ("running anda update for " <> show (length wanted) <> " scripts oglyad can't follow, " <> show (length wanted - length misses) <> " cached")
          env <- ask
          r <- liftIO $ withScratchCopy root $ \copy ->
            projectNames copy >>= \case
              Left e -> pure (Left e)
              Right names -> fmap Right . flip runReaderT env . forConcurrentlyN cfgConcurrency misses $ \p ->
                liftIO $
                  (pkgPath p,) <$> case (Map.lookup (pkgPath p) names, pkgSpecPath p) of
                    (Just name, Just spec) ->
                      updateProject copy cfgGitHubToken cfgBranch name >>= \case
                        Left e -> pure (Left e)
                        Right () ->
                          either (Left . ("rpmspec failed on the updated spec: " <>)) (Right . evrVersion . siEVR)
                            <$> querySpec cfgRelease cfgDist (copy </> spec)
                    _ -> pure (Left "anda doesn't list this project")
          pure $ case join' r of
            Left e -> ([(collectErr "anda" e) {fSeverity = Warn}], Map.empty)
            Right xs -> ([], Map.fromList xs)
  let answer p = maybe (Map.lookup (pkgPath p) ran) (Just . ceResult) (fresh p)
      new =
        Map.fromList
          [ (pkgFingerprint p, CacheEntry now (Right v))
          | p <- misses,
            Just (Right v) <- [Map.lookup (pkgPath p) ran]
          ]
      recent = Map.filter (\e -> diffUTCTime now (ceFetched e) < 30 * nominalDay) cache
  liftIO (saveJson cacheFile (new <> recent))
  pure
    ( problems,
      [ case answer p of
          Just (Right v) -> p {pkgUpstreamVer = Just v}
          Just (Left e) -> p {pkgUpstreamErr = Just ("update.rhai failed under anda: " <> e)}
          Nothing -> p
      | p <- pkgs
      ]
    )
  where
    join' = either Left id

collectErr :: Text -> Text -> Finding
collectErr what msg = Finding {fCheck = "collect", fPackage = what, fPath = Nothing, fSeverity = Error, fMessage = msg, fReleases = []}
