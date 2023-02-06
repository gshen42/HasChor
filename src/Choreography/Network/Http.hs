{-# LANGUAGE DataKinds #-}

module Choreography.Network.Http where

import Choreography.Location
import Choreography.Network
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
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Control.Monad ((>=>))

type API = "send" :> Capture "from" Location :> ReqBody '[PlainText] String :> PostNoContent

instance ToHttpApiData Location where
  toUrlPiece (Location s) = toUrlPiece s

instance FromHttpApiData Location where
  parseUrlPiece = parseUrlPiece >=> (return . Location)

data Config = Config
  { locToUrl :: HashMap Location BaseUrl
  }

type Host = String
type Port = Int

mkConfig :: [(Location, (Host, Port))] -> Config
mkConfig = Config . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

runNetwork :: Config -> Location -> Network a -> IO a
runNetwork cfg self prog = do
  ctx <- mkContext (HashMap.keys $ locToUrl cfg)
  -- TODO: kill the threads when the main thread exits
  -- use `bracket` or `withAsync`
  forkIO $ sendThread cfg ctx
  forkIO $ recvThread cfg ctx
  interpNetwork ctx prog
  where
    api :: Proxy API
    api = Proxy

    send :: Location -> String -> ClientM NoContent
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
        handler :: Location -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (recvChans ctx ! rmt) msg
          return NoContent

    recvThread :: Config -> Context -> IO ()
    recvThread cfg ctx = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server ctx)
