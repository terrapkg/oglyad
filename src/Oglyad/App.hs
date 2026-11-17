module Oglyad.App
  ( Env (..),
    App,
    HttpError (..),
    runApp,
    inRelease,
    httpGetE,
    forConcurrentlyN,
    processSlot,
    lookupOnce,
    note,
    oglyadVersion,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, forConcurrently)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, throwIO, try)
import Control.Monad.Reader (ReaderT, ask, asks, liftIO, local, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (requestHeaders, shouldStripHeaderOnRedirect),
    Response,
    brReadSome,
    parseUrlThrow,
    responseBody,
    responseHeaders,
    responseStatus,
    withResponse,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAuthorization, statusCode)
import Oglyad.Types (Config (..))
import Paths_oglyad (version)
import System.IO (hPutStrLn, stderr)

data Env = Env
  { envConfig :: Config,
    envHttp :: Manager,
    -- | crates.io allows one request per second
    envCratesLock :: MVar (),
    -- | GitHub's secondary rate limits punish lots of concurrent requests
    envGitHubSem :: QSem,
    -- | set once GitHub rate-limits us, so the remaining lookups are skipped
    -- instead of each failing and being reported separately
    envGitHubLimited :: IORef (Maybe Text),
    envLogLock :: MVar (),
    envLogPrefix :: String,
    envProcessSem :: QSem,
    envLookups :: MVar (Map Text (Async (Either HttpError Text))),
    envCacheLock :: MVar ()
  }

type App = ReaderT Env IO

data HttpError
  = -- | not the upstream's fault, so not cached and not reported per package
    RateLimited Text
  | HttpFailed Text
  deriving stock (Eq, Show)

oglyadVersion :: Text
oglyadVersion = T.pack (showVersion version)

-- | identifies us to upstream APIs (crates.io's policy asks for this)
userAgent :: ByteString
userAgent = TE.encodeUtf8 ("oglyad/" <> oglyadVersion <> " (+https://github.com/terrapkg/oglyad)")

-- | Fedora's compressed primary.xml is the biggest thing we fetch, at around 30MB
maxBodyMiB :: Int
maxBodyMiB = 256

maxBody :: Int
maxBody = maxBodyMiB * 1024 * 1024

runApp :: Config -> App a -> IO a
runApp cfg act = do
  mgr <- newTlsManager
  lock <- newMVar ()
  ghSem <- newQSem 6
  limited <- newIORef Nothing
  logLock <- newMVar ()
  procSem <- newQSem (max 1 (cfgConcurrency cfg))
  lookups <- newMVar Map.empty
  cacheLock <- newMVar ()
  runReaderT act (Env cfg mgr lock ghSem limited logLock "" procSem lookups cacheLock)

inRelease :: Config -> String -> App a -> App a
inRelease cfg prefix = local (\env -> env {envConfig = cfg, envLogPrefix = prefix})

-- | progress messages go to stderr so stdout stays clean for --json
note :: String -> App ()
note msg = do
  Env {envLogLock, envLogPrefix} <- ask
  liftIO (withMVar envLogLock (\_ -> hPutStrLn stderr ("oglyad: " <> envLogPrefix <> msg)))

isGitHubApi :: Text -> Bool
isGitHubApi = T.isPrefixOf "https://api.github.com/"

httpGet :: Text -> App LBS.ByteString
httpGet url = do
  Env {..} <- ask
  req <- liftIO (parseUrlThrow (T.unpack url))
  let auth = case cfgGitHubToken envConfig of
        Just t | isGitHubApi url -> [(hAuthorization, "Bearer " <> TE.encodeUtf8 t)]
        _ -> []
      req' =
        req
          { requestHeaders = ("User-Agent", userAgent) : auth <> requestHeaders req,
            shouldStripHeaderOnRedirect = (== hAuthorization)
          }
      fetch = withResponse req' envHttp $ \resp -> do
        body <- brReadSome (responseBody resp) (maxBody + 1)
        if LBS.length body > fromIntegral maxBody
          then throwIO (userError ("response from " <> T.unpack url <> " is over " <> show maxBodyMiB <> "MiB"))
          else pure body
  liftIO $
    if
      | "https://crates.io/" `T.isPrefixOf` url -> withMVar envCratesLock (\_ -> fetch <* threadDelay 1_000_000)
      | isGitHubApi url -> bracket_ (waitQSem envGitHubSem) (signalQSem envGitHubSem) fetch
      | otherwise -> fetch

-- | a short rate limit gets one retry. a longer GitHub one stops all further
-- GitHub requests for this run
httpGetE :: Text -> App (Either HttpError LBS.ByteString)
httpGetE url = do
  env@Env {..} <- ask
  limited <- liftIO (readIORef envGitHubLimited)
  case limited of
    Just why | isGitHubApi url -> pure (Left (RateLimited why))
    _ -> liftIO (attempt env (1 :: Int))
  where
    attempt env@Env {..} n = do
      r <- try (runReaderT (httpGet url) env)
      case r of
        Right body -> pure (Right body)
        Left (HttpExceptionRequest _ (StatusCodeException resp _)) -> do
          wait <- rateLimitWait resp
          case wait of
            Just secs | n == 1 && secs <= longestRetryWait -> do
              threadDelay ((secs + 1) * 1_000_000)
              attempt env (n + 1)
            Just _ -> do
              let why = "rate limited by " <> host
              if isGitHubApi url then writeIORef envGitHubLimited (Just why) else pure ()
              pure (Left (RateLimited why))
            Nothing -> pure (Left (HttpFailed ("HTTP " <> T.pack (show (statusCode (responseStatus resp))) <> " from " <> url)))
        Left (HttpExceptionRequest _ content) -> pure (Left (HttpFailed (T.pack (show content) <> " from " <> url)))
        Left (InvalidUrlException u why) -> pure (Left (HttpFailed ("invalid url " <> T.pack u <> ": " <> T.pack why)))
    host = T.takeWhile (/= '/') (T.drop (T.length "https://") url)
    longestRetryWait = 60

-- | seconds until a rate limit lifts. GitHub signals its primary limit with a
-- 403 and x-ratelimit-remaining: 0, and its secondary limit with a 403 or 429
-- plus retry-after
rateLimitWait :: Response () -> IO (Maybe Int)
rateLimitWait resp = do
  now <- floor <$> getPOSIXTime
  let header k = lookup k (responseHeaders resp)
      num = fmap fst . (BS8.readInt =<<)
      status = statusCode (responseStatus resp)
      retryAfter = num (header "retry-after")
      untilReset = (\t -> max 0 (t - now)) <$> num (header "x-ratelimit-reset")
      exhausted = header "x-ratelimit-remaining" == Just "0"
  pure $
    if
      | status == 429 || (status == 403 && (exhausted || retryAfter /= Nothing)) ->
          Just (fromMaybe 3600 (retryAfter <|> untilReset))
      | otherwise -> Nothing

forConcurrentlyN :: Int -> [a] -> (a -> App b) -> App [b]
forConcurrentlyN n xs f = do
  env <- ask
  liftIO $ do
    sem <- newQSem (max 1 n)
    forConcurrently xs $ \x ->
      bracket_ (waitQSem sem) (signalQSem sem) (runReaderT (f x) env)

processSlot :: App (IO a -> IO a)
processSlot = do
  sem <- asks envProcessSem
  pure (bracket_ (waitQSem sem) (signalQSem sem))

lookupOnce :: Text -> App (Either HttpError Text) -> App (Either HttpError Text)
lookupOnce key act = do
  env <- ask
  liftIO $ do
    running <- modifyMVar (envLookups env) $ \m -> case Map.lookup key m of
      Just inFlight -> pure (m, inFlight)
      Nothing -> do
        started <- async (runReaderT act env)
        pure (Map.insert key started m, started)
    Async.wait running
