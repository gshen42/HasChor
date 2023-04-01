{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

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
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp (run)

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String
type Port = Int

mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

type RecvChans = HashMap LocTm (Chan String)

mkRecvChans :: HttpConfig -> IO RecvChans
mkRecvChans cfg = foldM f HashMap.empty (locs cfg)
  where
    f :: HashMap LocTm (Chan String) -> LocTm
      -> IO (HashMap LocTm (Chan String))
    f hm l = do
      c <- newChan
      return $ HashMap.insert l c hm

runNetworkHttp :: MonadIO m => HttpConfig -> LocTm -> Network m a -> m a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftIO $ mkRecvChans cfg
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- runNetworkMain mgr chans prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: MonadIO m => Manager -> RecvChans -> Network m a -> m a
    runNetworkMain mgr chans = interpFreer handler
      where
        handler :: MonadIO m => NetworkSig m a -> m a
        handler (Run m)    = m
        handler(Send a l) = liftIO $ do
          res <- runClientM (send self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
          case res of
            Left err -> putStrLn $ "Error : " ++ show err
            Right _  -> return ()
        handler (Recv l)   = liftIO $ read <$> readChan (chans ! l)
        handler (BCast a)  = mapM_ handler $ fmap (Send a) (locs cfg)

    api :: Proxy API
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
        handler :: LocTm -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (chans ! rmt) msg
          return NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg chans = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server chans)

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
