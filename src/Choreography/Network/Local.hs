--------------------------------------------------------------------------------
-- Don't use this module, it's not done.
--------------------------------------------------------------------------------

module Choreography.Network.Local where

import Choreography.Location
import Choreography.Network
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (HashMap, (!))

newtype LocalConfig = LocalConfig Context

mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig = mkContext >=> (return . LocalConfig)

runNetworkLocal :: MonadIO m => LocalConfig -> LocTm -> Network m a -> m a
runNetworkLocal cfg self prog = do
  let (LocalConfig ctx) = cfg
  liftIO $ forkIO $ sendRecvThrd ctx
  runNetworkMain ctx prog
  loop
  where
    loop = loop

    sendRecvThrd :: Context -> IO ()
    sendRecvThrd ctx = do
      (rmt, msg) <- readChan (sendChan ctx)
      writeChan (recvChans ctx ! rmt) msg

instance Backend LocalConfig where
  runNetwork = runNetworkLocal
