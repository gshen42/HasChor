{-# LANGUAGE DataKinds #-}

module Choreography.Network.Http where

import Choreography.Location
import Choreography.Network hiding (run)
import Data.ByteString (fromStrict)
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API
import Servant.Client (ClientM, client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))
import Servant.Server (Handler, Server, serve)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (writeChan, readChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai.Handler.Warp (run)
import Control.Monad ((>=>))

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

data Config = Config
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String
type Port = Int

mkConfig :: [(LocTm, (Host, Port))] -> Config
mkConfig = Config . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

runNetwork :: MonadIO m => Config -> LocTm -> Network m a -> m a
runNetwork cfg self prog = do
  ctx <- liftIO $ mkContext (HashMap.keys $ locToUrl cfg)
  liftIO $ forkIO (sendThread cfg ctx)
  liftIO $ forkIO (recvThread cfg ctx)
  runNetworkMain ctx prog
  loop -- TODO: the `loop` here is to ensure the main thread only exits when
       -- the sending thread has finished sending all the messages
  where
    loop = loop

    api :: Proxy API
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    sendThread :: Config -> Context -> IO ()
    sendThread cfg ctx = do
      mgr <- newManager defaultManagerSettings
      (rmt, msg) <- readChan (sendChan ctx)
      res <- runClientM (send self msg) (mkClientEnv mgr (locToUrl cfg ! rmt))
      case res of
        Left err -> putStrLn $ "Error : " ++ show err
        Right _ -> sendThread cfg ctx

    server :: Context -> Server API
    server ctx = handler
      where
        handler :: LocTm -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (recvChans ctx ! rmt) msg
          return NoContent

    recvThread :: Config -> Context -> IO ()
    recvThread cfg ctx = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server ctx)

instance Backend Config where
  runNetwork = Choreography.Network.Http.runNetwork
