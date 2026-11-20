module Oglyad.Collect
  ( buildSnapshot,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (withMVar)
import Control.Applicative ((<|>))
import Control.Monad (forM, join)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Data.Either (partitionEithers)
import Data.List (nub, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Oglyad.App (App, Env (..), HttpError (..), forConcurrentlyN, lookupOnce, note, processSlot)
import Oglyad.Source.Anda (projectNames, reportSupported, updateProject, withScratchCopy)
import Oglyad.Source.Cache (AndaEntry (..), CacheEntry (..), UpstreamCache, loadJson, lookupFresh, saveJson)
import Oglyad.Source.Monorepo
import Oglyad.Source.Repodata (fetchRepo)
import Oglyad.Source.Rhai (parseUpdateRhaiWith, tokenize, tracksSubmodules)
import Oglyad.Source.RhaiEval (Simulated (..), simulateVersion)
import Oglyad.Source.Upstream (latestVersion)
import Oglyad.Types
import System.Directory (findExecutable, makeAbsolute)
import System.FilePath ((</>))
import System.Info qualified

-- | problems along the way become findings instead of crashes, so one broken
-- spec doesn't take down the whole run.
--
-- the monorepo work (rpmspec, git) is all local cpu and the repodata work is
-- all network, so the two halves run at the same time
buildSnapshot :: App (Snapshot, [Finding])
buildSnapshot = do
  cfg@Config {..} <- asks envConfig
  now <- liftIO getCurrentTime
  tree <- liftIO $ case cfgSpecRef of
    Nothing -> Right . (cfgMonorepo,"HEAD",) <$> headCommit cfgMonorepo
    Just ref -> fmap (\(dir, sha) -> (dir, sha, Just sha)) <$> checkoutRef cfgMonorepo ref
  let (root, logRef, hd, treeErrs) = case tree of
        Right (r, l, h) -> (Just r, l, h, [])
        Left err -> (Nothing, "HEAD", Nothing, [collectErr (fromMaybe "monorepo" cfgSpecRef) err])
  (dirErrs, dirs) <- partitionEithers <$> liftIO (maybe (pure []) (`findPackageDirs` cfgRoot) root)

  let subrepos = if cfgSubrepos then nub (map Just (mapMaybe pdSubrepo dirs)) else []
      terraRepos = [(s, repoUrl cfg s) | s <- Nothing : subrepos]
      publishedTo p = if cfgSubrepos then pkgSubrepo p else Nothing
  env <- ask
  ((specErrs, pkgs0), (terra, base)) <-
    liftIO $
      concurrently
        (runReaderT (readPackages (fromMaybe cfgMonorepo root) logRef hd dirs) env)
        (runReaderT (fetchRepos terraRepos cfgBaseRepos) env)

  let (repoErrs, repos) = partitionEithers terra
      fetched = Map.fromList repos
      pkgs1 =
        [ p
            { pkgRepoBuild = Map.lookup (publishedTo p) fetched >>= Map.lookup (pkgName p),
              pkgRepoChecked = Map.member (publishedTo p) fetched
            }
        | p <- pkgs0
        ]
  (upErrs, withUpstream) <- if cfgUpstream then upstreamVersions now root pkgs1 else pure ([], id)
  let pkgs = map withUpstream pkgs1

  let snap =
        Snapshot
          { snapPackages = Map.fromList [(pkgPath p, p) | p <- pkgs],
            snapBase = foldMap Map.keysSet base,
            snapNow = now,
            snapConfig = cfg,
            snapHead = hd,
            snapIgnoreLabels = Map.fromList [(pdPath d, pdIgnores d) | d <- dirs, not (null (pdIgnores d))]
          }
      unreadable =
        [ (collectErr (T.pack path) ("can't read package dir: " <> err)) {fPath = Just (T.pack path)}
        | (path, err) <- dirErrs
        ]
  pure (snap, treeErrs <> unreadable <> specErrs <> repoErrs <> upErrs)

upstreamVersions :: UTCTime -> Maybe FilePath -> [Package] -> App ([Finding], Package -> Package)
upstreamVersions now root pkgs = do
  env <- ask
  ((upErrs, fromApis), (andaErrs, fromAnda)) <-
    liftIO $
      concurrently
        (runReaderT (withUpstreams now pkgs) env)
        (runReaderT (maybe (pure ([], id)) (\r -> withAnda r now pkgs) root) env)
  pure (upErrs <> andaErrs, fromAnda . fromApis)

-- | Terra failures become findings, base distro ones just mean no overlap
-- data
fetchRepos ::
  [(Maybe Text, Text)] ->
  [Text] ->
  App ([Either Finding (Maybe Text, Map Text RepoBuild)], [Map Text RepoBuild])
fetchRepos terraRepos baseRepos = do
  let jobs = map Left terraRepos <> map Right baseRepos
  results <- forConcurrentlyN (length jobs) jobs $ \case
    Left (sub, url) -> Left . either (Left . collectErr url . T.pack) (\b -> Right (sub, b)) <$> fetchRepo url
    Right url -> Right . either (const Map.empty) id <$> fetchRepo url
  pure (partitionEithers results)

readPackages :: FilePath -> Text -> Maybe Text -> [PkgDir] -> App ([Finding], [Package])
readPackages root0 logRef headSha dirs = do
  Config {..} <- asks envConfig
  root <- liftIO (makeAbsolute root0)
  -- one cache file per release, so cron jobs checking different releases
  -- don't keep evicting each other's entries
  let cacheFile = "specs" <> T.unpack cfgCacheTag <> ".json"
  (commits, (macros, cache)) <-
    liftIO $
      concurrently
        (commitTimes cfgMonorepo cfgRoot logRef headSha cfgCacheTag (map pdPath dirs))
        ((,) <$> macroFingerprint <*> (fromMaybe Map.empty <$> loadJson cacheFile))
  keyed <- forConcurrentlyN cfgConcurrency dirs $ \d -> liftIO $ do
    key <- specFingerprint macros cfgMacros root (pdPath d)
    pure (key, d)
  let misses = length [() | (k, _) <- keyed, isNothing (Map.lookup k (cache :: Map Text (Either Text SpecInfo)))]
  note ("evaluating " <> show (length dirs) <> " specs, " <> show (length dirs - misses) <> " unchanged since last run")
  -- without rpmspec, cached evaluations still work. everything else is
  -- skipped and reported once, rather than as one error per package
  hasRpmspec <- liftIO (isJust <$> findExecutable "rpmspec")
  inSlot <- processSlot
  results <- forConcurrentlyN cfgConcurrency keyed $ \(key, d) -> liftIO $ do
    case (Map.lookup key cache, pdSpec d) of
      (Just cached, _) -> pure (Evaluated key d cached)
      (Nothing, Just spec)
        | hasRpmspec -> Evaluated key d <$> inSlot (querySpec cfgMacros (root </> spec))
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
          [toPackage commits k d (Just i) | Evaluated k d i <- results, not (failedForThisHost d i)]
            <> [toPackage commits "" d Nothing | NoSpec d <- results]
  pure (missing <> errs, pkgs)

data SpecResult
  = Evaluated Text PkgDir (Either Text SpecInfo)
  | NoSpec PkgDir
  | Skipped

failedForThisHost :: PkgDir -> Either Text SpecInfo -> Bool
failedForThisHost PkgDir {pdArches} = \case
  Left err -> "No compatible architectures found for build" `T.isInfixOf` err && not buildsHere
  Right _ -> False
  where
    buildsHere = null pdArches || T.pack System.Info.arch `elem` pdArches

-- | uncommitted changes can't change any "last commit", so the commit is the
-- whole cache key. one cache file per release, since each reads its own branch
commitTimes :: FilePath -> FilePath -> Text -> Maybe Text -> Text -> [FilePath] -> IO (Map FilePath UTCTime)
commitTimes mono pkgRoot ref hd tag dirs = do
  let file = "commits" <> T.unpack tag <> ".json"
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
          pkgUpdBranch = pdUpdBranch,
          pkgSpecPath = pdSpec,
          pkgFingerprint = key,
          pkgSpecEVR = Just siEVR,
          pkgRepoBuild = Nothing,
          pkgLastCommit = Map.lookup pdPath commits,
          pkgUpstream = parseUpdateRhaiWith pdVersionMacros <$> (pdUpdateRhai >>= unlessOnlyComments),
          pkgRhai = pdUpdateRhai,
          pkgUpstreamVer = Nothing,
          pkgUpstreamErr = Nothing,
          pkgSubmoduleEvents = [],
          pkgRepoChecked = False
        }
  where
    firstLine t = let l = T.takeWhile (/= '\n') t in if T.null l then "(no output)" else l
    unlessOnlyComments script = case tokenize script of
      Just [] -> Nothing
      _ -> Just script
    pathErr msg = (collectErr (T.pack pdPath) msg) {fPath = Just (T.pack pdPath)}

-- | crates.io allows one request a second, so its lookups run in their own
-- serial queue next to everything else. inside the shared job pool, a cluster
-- of crates packages would hold every slot while waiting their turn
withUpstreams :: UTCTime -> [Package] -> App ([Finding], Package -> Package)
withUpstreams now pkgs = do
  cfg@Config {..} <- asks envConfig
  cache <- liftIO (fromMaybe Map.empty <$> loadJson "upstream.json") :: App UpstreamCache
  let wanted = [p | p <- pkgs, Just src <- [pkgUpstream p], lookable src]
      fresh p = pkgUpstream p >>= \src -> lookupFresh (cacheMaxAge cfg now) now src cache
      (crates, others) = partition (isCrates . pkgUpstream) wanted
      lookup1 p = case (fresh p, pkgUpstream p) of
        (Just e, _) -> pure (pkgPath p, either (Left . HttpFailed) Right (ceResult e))
        (Nothing, Just src) -> (pkgPath p,) <$> lookupOnce (cacheKey src) (latestVersion src)
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
          [ (cacheKey src, CacheEntry now (Right v))
          | p <- wanted,
            isNothing (fresh p),
            Just src <- [pkgUpstream p],
            Just (Right v) <- [Map.lookup (pkgPath p) byPath]
          ]
      limited = [why | Left (RateLimited why) <- Map.elems byPath]
      rateErr = case limited of
        [] -> []
        why : _ ->
          [ (collectErr "upstream" (why <> ", skipped " <> T.pack (show (length limited)) <> " lookups. set GITHUB_TOKEN, or try again later"))
              { fSeverity = Warn
              }
          ]
  saveSharedCache "upstream.json" now new
  pure
    ( rateErr,
      \p -> case Map.lookup (pkgPath p) byPath of
        Just (Right v) -> p {pkgUpstreamVer = replay p v}
        Just (Left (HttpFailed e)) -> p {pkgUpstreamErr = Just e}
        -- rate limits are reported once above, not as a broken upstream
        Just (Left (RateLimited _)) -> p
        Nothing -> p
    )
  where
    cacheKey src = T.pack (show src)
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

withAnda :: FilePath -> UTCTime -> [Package] -> App ([Finding], Package -> Package)
withAnda root now pkgs = do
  cfg@Config {..} <- asks envConfig
  let wanted =
        [ p
        | p@Package {pkgSpecPath = Just _} <- pkgs,
          unfollowable p || maybe False tracksSubmodules (pkgRhai p),
          cfgRolling || pkgUpdBranch p
        ]
      cacheFile = "anda" <> T.unpack cfgCacheTag <> ".json"
  cache <- liftIO (fromMaybe Map.empty <$> loadJson cacheFile) :: App (Map Text AndaEntry)
  let fresh p = case Map.lookup (pkgFingerprint p) cache of
        Just e | diffUTCTime now (aeFetched e) <= cacheMaxAge cfg now -> Just e
        _ -> Nothing
      misses = [p | p <- wanted, isNothing (fresh p)]
  hasAnda <- liftIO (isJust <$> findExecutable "anda")
  andaReports <- if hasAnda && not (null misses) then liftIO reportSupported else pure False
  (problems, ran) <-
    if
      | null misses -> pure ([], Map.empty)
      | not hasAnda -> do
          note ("anda not found, so " <> show (length misses) <> " update.rhai scripts that need anda are skipped")
          pure ([], Map.empty)
      | not andaReports ->
          pure ([(collectErr "anda" ("anda is too old for `anda update --report`, so " <> T.pack (show (length misses)) <> " update.rhai scripts that need anda are skipped. update anda")) {fSeverity = Warn}], Map.empty)
      | otherwise -> do
          note ("running anda update for " <> show (length wanted) <> " scripts that need anda, " <> show (length wanted - length misses) <> " cached")
          env <- ask
          inSlot <- processSlot
          r <- liftIO $ withScratchCopy root $ \copy ->
            projectNames copy >>= \case
              Left e -> pure (Left e)
              Right names -> fmap Right . flip runReaderT env . forConcurrentlyN cfgConcurrency misses $ \p ->
                liftIO . inSlot $
                  (pkgPath p,) <$> case (Map.lookup (pkgPath p) names, pkgSpecPath p) of
                    (Just name, Just spec) ->
                      updateProject copy cfgGitHubToken cfgBranch name >>= \case
                        Left e -> pure (Left e)
                        Right events ->
                          either (Left . ("rpmspec failed on the updated spec: " <>)) (\i -> Right (evrVersion (siEVR i), events))
                            <$> querySpec cfgMacros (copy </> spec)
                    _ -> pure (Left "anda doesn't list this project")
          pure $ case join r of
            Left e -> ([(collectErr "anda" e) {fSeverity = Warn}], Map.empty)
            Right xs -> ([], Map.fromList xs)
  let answer p = maybe (Map.lookup (pkgPath p) ran) (\e -> Just ((,aeEvents e) <$> aeResult e)) (fresh p)
      new =
        Map.fromList
          [ (pkgFingerprint p, AndaEntry now (Right v) events)
          | p <- misses,
            Just (Right (v, events)) <- [Map.lookup (pkgPath p) ran]
          ]
  liftIO (saveJson cacheFile (new <> Map.filter ((< 30 * nominalDay) . diffUTCTime now . aeFetched) cache))
  pure
    ( problems,
      \p -> case answer p of
        Just (Right (v, events))
          | unfollowable p -> p {pkgUpstreamVer = Just v, pkgSubmoduleEvents = events}
          | otherwise -> p {pkgSubmoduleEvents = events}
        Just (Left e) -> p {pkgUpstreamErr = pkgUpstreamErr p <|> Just ("update.rhai failed under anda: " <> e)}
        Nothing -> p
    )
  where
    unfollowable p = case pkgUpstream p of
      Just (Unknown _) -> True
      _ -> False

-- | anything fetched since this run started is fresh, so checking several
-- releases asks each upstream once even with --cache-hours 0
cacheMaxAge :: Config -> UTCTime -> NominalDiffTime
cacheMaxAge Config {..} now = max cfgCacheFor (diffUTCTime now cfgRunStart)

fetchedThisMonth :: UTCTime -> CacheEntry -> Bool
fetchedThisMonth now e = diffUTCTime now (ceFetched e) < 30 * nominalDay

saveSharedCache :: FilePath -> UTCTime -> Map Text CacheEntry -> App ()
saveSharedCache file now new = do
  lock <- asks envCacheLock
  liftIO . withMVar lock $ \_ -> do
    onDisk <- fromMaybe Map.empty <$> loadJson file
    saveJson file (new <> Map.filter (fetchedThisMonth now) onDisk)

collectErr :: Text -> Text -> Finding
collectErr what msg = Finding {fCheck = "collect", fPackage = what, fPath = Nothing, fSeverity = Error, fMessage = msg, fReleases = []}
