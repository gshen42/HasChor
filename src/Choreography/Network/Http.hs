{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.Network.Http where

import Choreography.Location
import Choreography.Network hiding (run)
import Data.ByteString (fromStrict)
import Data.Hashable (Hashable(..))
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API hiding (Host)
import Servant.Client (ClientM, client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))
import Servant.Server (Handler, Server, serve)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Tree
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp (run)

-- * Servant API

type API = "send"
 :> Capture "from" LocTm
 :> Capture "session" SessionId
 :> ReqBody '[PlainText] String :> PostNoContent

instance ToHttpApiData SessionId where
  toUrlPiece = Text.pack . go
    where
      go :: SessionId -> String
      go Root = "root"
      go (Nest (Left ()) sid) = go sid ++ "0"
      go (Nest (Right ()) sid) = go sid ++ "1"

instance FromHttpApiData SessionId where
  parseUrlPiece t =
    case Text.unpack t of
      'r' : 'o' : 'o' : 't' : rest -> foldM step Root rest
      _ -> Left (Text.pack "Invalid SessionId: expected prefix 'root'")
    where
      step :: SessionId -> Char -> Either Text SessionId
      step sid '0' = Right (Nest (Left ()) sid)
      step sid '1' = Right (Nest (Right ()) sid)
      step _ c = Left (Text.pack ("Invalid SessionId branch: " ++ [c]))

-- * Http configuration

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
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receiving channels

instance Hashable SessionId where
  hashWithSalt salt Root = hashWithSalt salt (0 :: Int)
  hashWithSalt salt (Nest (Left ()) sid) = hashWithSalt (hashWithSalt salt (1 :: Int)) sid
  hashWithSalt salt (Nest (Right ()) sid) = hashWithSalt (hashWithSalt salt (2 :: Int)) sid

type RecvChans = MVar (HashMap (LocTm, SessionId) (Chan String))

mkRecvChans :: HttpConfig -> IO RecvChans
mkRecvChans _ = newMVar HashMap.empty

lookupRecvChan :: RecvChans -> LocTm -> SessionId -> IO (Chan String)
lookupRecvChan chans loc sid =
  modifyMVar chans $ \hm ->
    case HashMap.lookup (loc, sid) hm of
      Just chan -> pure (hm, chan)
      Nothing -> do
        chan <- newChan
        let hm' = HashMap.insert (loc, sid) chan hm
        pure (hm', chan)

-- * HTTP backend

-- IO monad but the `Applicative` instance runs computations concurrently
newtype ConIO a = ConIO { unConIO :: IO a }

deriving instance Functor ConIO
deriving instance Monad ConIO
deriving instance MonadIO ConIO

instance Applicative ConIO where
  f <*> a = ConIO $ do
    (f', a') <- concurrently (unConIO f) (unConIO a)
    return (f' a')

  pure = ConIO . pure

runNetworkHttp :: HttpConfig -> LocTm -> Network IO a -> IO a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftIO $ mkRecvChans cfg
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- unConIO $ runNetworkMain mgr chans prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: Manager -> RecvChans -> Network IO a -> ConIO a
    runNetworkMain mgr chans = interp handler
      where
        handler :: NetworkSig IO a -> ConIO a
        handler (Exec m) = liftIO m
        handler (Send sid a l) = liftIO $ do
          res <- runClientM (send self sid $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
          case res of
            Left err -> putStrLn $ "Error : " ++ show err
            Right _  -> return ()
        handler (Recv sid l) = liftIO $ do
          chan <- lookupRecvChan chans l sid
          read <$> readChan chan
        handler (BCast sid a) = mapM_ handler $ fmap (Send sid a) (locs cfg)

    api :: Proxy API
    api = Proxy

    send :: LocTm -> SessionId -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
        handler :: LocTm -> SessionId -> String -> Handler NoContent
        handler rmt sid msg = do
          chan <- liftIO $ lookupRecvChan chans rmt sid
          liftIO $ writeChan chan msg
          return NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg chans = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server chans)

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
