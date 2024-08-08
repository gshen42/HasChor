{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.NetworkAsync.Http where

import Choreography.Location (LocTm)
import Choreography.NetworkAsync hiding (run)
import Control.Concurrent
import Control.Concurrent.Async (Async, async)
import Control.Monad (void)
import Control.Monad.Freer (Freer, interpFreer)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (isRight)
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy (..))
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client
import Servant.Server (Handler, Server, serve)

-- * HTTP configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String

type Port = Int

-- | Create a HTTP backend configuration from a association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = host,
          baseUrlPort = port,
          baseUrlPath = ""
        }

-- | All locations of a HTTP configuration.
locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receive buffer

newtype RecvBuf = RecvBuf (MVar (HashMap (LocTm, SeqId) (MVar String)))

newRecvBuf :: IO RecvBuf
newRecvBuf = RecvBuf <$> newMVar HashMap.empty

put :: String -> (LocTm, SeqId) -> RecvBuf -> IO ()
put s id (RecvBuf buf) = do
  map <- takeMVar buf
  case HashMap.lookup id map of
    Just m -> do
      putMVar buf map
      putMVar m s
    Nothing -> do
      m <- newEmptyMVar
      putMVar buf (HashMap.insert id m map)
      putMVar m s

get :: (LocTm, SeqId) -> RecvBuf -> IO String
get id (RecvBuf buf) = do
  map <- takeMVar buf
  case HashMap.lookup id map of
    Just m -> do
      putMVar buf map
      takeMVar m
    Nothing -> do
      m <- newEmptyMVar
      putMVar buf (HashMap.insert id m map)
      takeMVar m

-- * HTTP backend

-- | Servant API
type API = "send" :> Capture "from" LocTm :> Capture "with-id" SeqId :> ReqBody '[PlainText] String :> PostNoContent

runNetworkHttp :: (MonadIO m) => HttpConfig -> LocTm -> Network m a -> m a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  buf <- liftIO $ newRecvBuf
  recvT <- liftIO $ forkIO (recvThread cfg buf)
  result <- runNetworkMain mgr buf prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: (MonadIO m) => Manager -> RecvBuf -> Network m a -> m a
    runNetworkMain mgr buf = interpFreer handler
      where
        handler :: (MonadIO m) => NetworkSig m a -> m a
        handler (Run m) = m
        handler (Send a l id) = liftIO $ async $ void $ runClientM (sendServant self id (show a)) (mkClientEnv mgr (locToUrl cfg ! l))
        handler (Recv l id) = liftIO $ async $ read <$> get (l, id) buf
        handler (BCast a id) = traverse (handler . (\l -> Send a l id)) (locs cfg)

    recvThread :: HttpConfig -> RecvBuf -> IO ()
    recvThread cfg buf = run (baseUrlPort $ locToUrl cfg ! self) (serve api $ server buf)

    api :: Proxy API
    api = Proxy

    sendServant :: LocTm -> SeqId -> String -> ClientM NoContent
    sendServant = client api

    server :: RecvBuf -> Server API
    server buf = handler
      where
        handler :: LocTm -> SeqId -> String -> Handler NoContent
        handler rmt id msg = do
          liftIO $ put msg (rmt, id) buf
          return NoContent

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
