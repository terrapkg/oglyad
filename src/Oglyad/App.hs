-- | the app monad. plain ReaderT over IO is the boring, correct starting point.
-- reach for mtl classes or an effect library only once this actually hurts
module Oglyad.App
  ( Env (..)
  , App
  , runApp
  , httpGet
  , httpGetE
  , forConcurrentlyN
  , note
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, try)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client
  ( HttpException (..)
  , HttpExceptionContent (..)
  , Manager
  , Request (requestHeaders)
  , httpLbs
  , parseUrlThrow
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)
import Oglyad.Types (Config (..))
import System.IO (hPutStrLn, stderr)

data Env = Env
  { envConfig :: Config
  , envHttp :: Manager
  , envCratesLock :: MVar () -- ^ crates.io allows one request per second
  }

type App = ReaderT Env IO

runApp :: Config -> App a -> IO a
runApp cfg act = do
  mgr <- newTlsManager
  lock <- newMVar ()
  runReaderT act (Env cfg mgr lock)

-- | progress messages go to stderr so stdout stays clean for --json
note :: String -> App ()
note = liftIO . hPutStrLn stderr . ("oglyad: " <>)

-- | GET a url, throwing on non-2xx
httpGet :: Text -> App LBS.ByteString
httpGet url = do
  Env {..} <- ask
  req <- liftIO (parseUrlThrow (T.unpack url))
  let auth = case cfgGitHubToken envConfig of
        Just t | "https://api.github.com/" `T.isPrefixOf` url -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)]
        _ -> []
      req' = req {requestHeaders = ("User-Agent", "oglyad (Terra package health checker)") : auth <> requestHeaders req}
      fetch = responseBody <$> httpLbs req' envHttp
  liftIO $
    if "https://crates.io/" `T.isPrefixOf` url
      then withMVar envCratesLock (\_ -> fetch <* threadDelay 1_000_000)
      else fetch

-- | like 'httpGet' but HTTP failures come back as a short readable Left
httpGetE :: Text -> App (Either Text LBS.ByteString)
httpGetE url = do
  env <- ask
  r <- liftIO (try (runReaderT (httpGet url) env))
  pure $ case r of
    Right body -> Right body
    Left (HttpExceptionRequest _ (StatusCodeException resp _)) ->
      Left ("HTTP " <> T.pack (show (statusCode (responseStatus resp))) <> " from " <> url)
    Left (HttpExceptionRequest _ content) -> Left (T.pack (show content) <> " from " <> url)
    Left (InvalidUrlException u why) -> Left ("invalid url " <> T.pack u <> ": " <> T.pack why)

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

