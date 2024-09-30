{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.Network.Http where

import Choreography.Location
import Choreography.Network
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HM
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

-- | Create a HTTP backend configuration from an association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HM.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = host,
          baseUrlPort = port,
          baseUrlPath = ""
        }

-- * Message buffer

type MsgBuf = MVar (HashMap (LocTm, SeqId) (MVar String))

emptyMsgBuf :: IO MsgBuf
emptyMsgBuf = newMVar HM.empty

lookupMsgBuf :: (LocTm, SeqId) -> MsgBuf -> IO (MVar String)
lookupMsgBuf id buf = do
  map <- takeMVar buf
  case HM.lookup id map of
    Just mvar -> do
      putMVar buf map
      return mvar
    Nothing -> do
      mvar <- newEmptyMVar
      putMVar buf (HM.insert id mvar map)
      return mvar

putMsg :: (Show a) => a -> (LocTm, SeqId) -> MsgBuf -> IO ()
putMsg msg id buf = do
  mvar <- lookupMsgBuf id buf
  putMVar mvar (show msg)

getMsg :: (Read a) => (LocTm, SeqId) -> MsgBuf -> IO a
getMsg id buf = do
  mvar <- lookupMsgBuf id buf
  read <$> takeMVar mvar

-- * HTTP backend

-- the Servant API
type API =
  "send"
    :> Capture "src" LocTm
    :> Capture "sid" SeqId
    :> ReqBody '[PlainText] String
    :> PostNoContent

api :: Proxy API
api = Proxy

server :: MsgBuf -> Server API
server buf = handler
  where
    handler :: LocTm -> SeqId -> String -> Handler NoContent
    handler rmt id msg = do
      liftIO $ print ("* Http Backend: Received " ++ msg ++ " from " ++ rmt ++ " with sequence number " ++ show id)
      liftIO $ putMsg msg (rmt, id) buf
      return NoContent

sendServant :: LocTm -> SeqId -> String -> ClientM NoContent
sendServant = client api

data Ctx = Ctx
  { cfg :: HttpConfig,
    self :: LocTm,
    mgr :: Manager,
    buf :: MsgBuf
  }

logMsg :: String -> IO ()
logMsg msg = putStrLn ("* Http backend: " ++ msg)

runNetworkMain :: (MonadIO m) => Network m a -> RWST Ctx [Async ()] () m a
runNetworkMain prog = interp handler (unNetwork prog)
  where
    handler :: (MonadIO m) => NetworkSig m a -> RWST Ctx [Async ()] () m a
    handler (Lift act) = do
      lift act
    handler (Send a dst sid) = do
      liftIO $ logMsg ("Asynchronously send " ++ show a ++ " to " ++ dst ++ " with sequence number " ++ show sid)
      Ctx {cfg, self, mgr, buf} <- ask
      liftIO $ async $ do
        let env = mkClientEnv mgr (locToUrl cfg ! dst)
        response <- runClientM (sendServant self sid (show a)) env
        either (logMsg . show) (void . return) response
    handler (Recv src sid) = do
      liftIO $ logMsg ("Asynchronously wait for a message from " ++ src ++ " with sequence number " ++ show sid)
      Ctx {buf} <- ask
      liftIO $ async $ do
        read <$> getMsg (src, sid) buf

runNetworkHttp :: (MonadIO m) => HttpConfig -> LocTm -> Network m a -> m a
runNetworkHttp cfg self prog = do
  -- initialization
  mgr <- liftIO $ newManager defaultManagerSettings
  buf <- liftIO emptyMsgBuf
  let ctx = Ctx {cfg, self, mgr, buf}

  -- start the server thread
  let serverPort = baseUrlPort (locToUrl cfg ! self)
  let app = serve api (server buf)
  server <- liftIO $ forkIO $ run serverPort app
  -- start the main thread
  (result, _, sendings) <- runRWST (runNetworkMain prog) ctx ()

  -- cleanup
  forM_ sendings (liftIO . wait) -- wait for all sendings to finish
  liftIO $ killThread server

  return result

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
