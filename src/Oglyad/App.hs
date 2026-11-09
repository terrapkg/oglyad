-- | the app monad. plain ReaderT over IO is the boring, correct starting point.
-- reach for mtl classes or an effect library only once this actually hurts
module Oglyad.App
  ( Env (..),
    App,
    HttpError (..),
    runApp,
    httpGetE,
    forConcurrentlyN,
    note,
    oglyadVersion,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, throwIO, try)
import Control.Monad.Reader (ReaderT, ask, asks, liftIO, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
    -- instead of each failing (and being reported) separately
    envGitHubLimited :: IORef (Maybe Text),
    -- | serializes progress messages so concurrent jobs can't interleave lines
    envLogLock :: MVar ()
  }

type App = ReaderT Env IO

data HttpError
  = -- | the server said slow down. not the upstream's fault, so not cached
    -- and not reported per package
    RateLimited Text
  | HttpFailed Text
  deriving stock (Eq, Show)

oglyadVersion :: Text
oglyadVersion = T.pack (showVersion version)

-- | identifies us to upstream APIs (crates.io's policy asks for this)
userAgent :: ByteString
userAgent = TE.encodeUtf8 ("oglyad/" <> oglyadVersion <> " (+https://github.com/halfcyan/oglyad)")

-- | no single response may be larger than this. Fedora's compressed
-- primary.xml is the biggest thing we fetch, at around 30MB
maxBody :: Int
maxBody = 256 * 1024 * 1024

runApp :: Config -> App a -> IO a
runApp cfg act = do
  mgr <- newTlsManager
  lock <- newMVar ()
  ghSem <- newQSem 6
  limited <- newIORef Nothing
  logLock <- newMVar ()
  runReaderT act (Env cfg mgr lock ghSem limited logLock)

-- | progress messages go to stderr so stdout stays clean for --json
note :: String -> App ()
note msg = do
  lock <- asks envLogLock
  liftIO (withMVar lock (\_ -> hPutStrLn stderr ("oglyad: " <> msg)))

isGitHubApi :: Text -> Bool
isGitHubApi = T.isPrefixOf "https://api.github.com/"

-- | GET a url, throwing on non-2xx
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
            -- never forward the token if a redirect goes somewhere else
            shouldStripHeaderOnRedirect = (== hAuthorization)
          }
      fetch = withResponse req' envHttp $ \resp -> do
        body <- brReadSome (responseBody resp) (maxBody + 1)
        if LBS.length body > fromIntegral maxBody
          then throwIO (userError ("response from " <> T.unpack url <> " is over 256MiB"))
          else pure body
  liftIO $
    if
      | "https://crates.io/" `T.isPrefixOf` url -> withMVar envCratesLock (\_ -> fetch <* threadDelay 1_000_000)
      | isGitHubApi url -> bracket_ (waitQSem envGitHubSem) (signalQSem envGitHubSem) fetch
      | otherwise -> fetch

-- | GET a url, with failures as a short readable Left instead of an
-- exception. a rate limit with a short Retry-After gets one retry. a GitHub
-- rate limit beyond that stops all further GitHub requests for this run
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
            Just secs | n == 1 && secs <= 60 -> do
              threadDelay ((secs + 1) * 1_000_000)
              attempt env (n + 1)
            Just _ -> do
              let why = "rate limited by " <> host
              if isGitHubApi url then writeIORef envGitHubLimited (Just why) else pure ()
              pure (Left (RateLimited why))
            Nothing -> pure (Left (HttpFailed ("HTTP " <> T.pack (show (statusCode (responseStatus resp))) <> " from " <> url)))
        Left (HttpExceptionRequest _ content) -> pure (Left (HttpFailed (T.pack (show content) <> " from " <> url)))
        Left (InvalidUrlException u why) -> pure (Left (HttpFailed ("invalid url " <> T.pack u <> ": " <> T.pack why)))
    host = T.takeWhile (/= '/') (T.drop 8 url)

-- | if this response is a rate limit, how many seconds until it lifts.
-- GitHub signals its primary limit with a 403 and x-ratelimit-remaining: 0,
-- and its secondary limit (too many concurrent requests) with a 403 or 429
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
          Just (maybe (maybe 3600 id untilReset) id retryAfter)
      | otherwise -> Nothing

-- | run an action over a list with at most n in flight at once.
-- 'forConcurrently' lives in IO, so we grab the env and re-run App inside each
-- thread. that dance is exactly what MonadUnliftIO (from unliftio) automates
forConcurrentlyN :: Int -> [a] -> (a -> App b) -> App [b]
forConcurrentlyN n xs f = do
  env <- ask
  liftIO $ do
    sem <- newQSem (max 1 n)
    forConcurrently xs $ \x ->
      bracket_ (waitQSem sem) (signalQSem sem) (runReaderT (f x) env)
