{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- This module provides the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
--------------------------------------------------------------------------------

module Choreography.Network where

import Choreography.Location
import Control.Concurrent.Chan
import Data.HashMap.Strict (HashMap, (!), insert, empty, keys)
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class

data NetworkSig m a where
  Run   :: m a -> NetworkSig m a
  Send  :: Show a => a -> LocTm -> NetworkSig m ()
  Recv  :: Read a => LocTm -> NetworkSig m a
  BCast :: Show a => a -> NetworkSig m ()

type Network m = Freer (NetworkSig m)

run :: m a -> Network m a
run m = toFreer $ Run m

send :: Show a => a -> LocTm -> Network m ()
send a l = toFreer $ Send a l

recv :: Read a => LocTm -> Network m a
recv l = toFreer $ Recv l

broadcast :: Show a => a -> Network m ()
broadcast a = toFreer $ BCast a

data Context = Context
  { sendChan :: Chan (LocTm, String)
  , recvChans :: HashMap LocTm (Chan String)
  }

mkContext :: [LocTm] -> IO Context
mkContext ls = do
  recvChans <- foldM f empty ls
  sendChan <- newChan
  return $ Context { recvChans = recvChans, sendChan = sendChan }
  where
    f :: HashMap LocTm (Chan String) -> LocTm
      -> IO (HashMap LocTm (Chan String))
    f hm l = do
      c <- newChan
      return $ insert l c hm

-- all the locations in a context, needed to implement broadcasting
locs :: Context -> [LocTm]
locs = keys . recvChans

-- interpet a `Network` program as a `IO` program with access to a `Context`
-- how `Context` is manipulated is determined in the next phase, this allows
-- us to have multiple implementations of `Network` while reusing most of the
-- code
runNetworkMain :: MonadIO m => Context -> Network m a -> m a
runNetworkMain ctx = runFreer alg
  where
    alg :: MonadIO m => NetworkSig m a -> m a
    alg (Run m)    = m
    alg (Send a l) = liftIO $ writeChan (sendChan ctx) (l, show a)
    alg (Recv l)   = liftIO $ read <$> readChan (recvChans ctx ! l)
    alg (BCast a)  = mapM_ alg $ fmap (Send a) (locs ctx)
