{-# LANGUAGE GADTs #-}

module Choreography.Network.Local where

import Choreography.Location
import Choreography.Network
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap

-- Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

newtype LocalConfig = LocalConfig
  { locToBuf :: HashMap LocTm MsgBuf
  }

newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      return (HashMap.insert loc chan hash)

mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig locs = LocalConfig <$> foldM f HashMap.empty locs
  where
    f hash loc = do
      buf <- newEmptyMsgBuf locs
      return (HashMap.insert loc buf hash)

locs :: LocalConfig -> [LocTm]
locs = HashMap.keys . locToBuf

runNetworkLocal :: MonadIO m => LocalConfig -> LocTm -> Network m a -> m a
runNetworkLocal cfg self prog = runFreer alg prog
  where
    alg :: MonadIO m => NetworkSig m a -> m a
    alg (Run m)    = m
    alg (Send a l) = liftIO $ writeChan ((locToBuf cfg ! l) ! self) (show a)
    alg (Recv l)   = liftIO $ read <$> readChan ((locToBuf cfg ! self) ! l)
    alg (BCast a)  = mapM_ alg $ fmap (Send a) (locs cfg)

instance Backend LocalConfig where
  runNetwork = runNetworkLocal

