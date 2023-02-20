--------------------------------------------------------------------------------
-- This module provides the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
--------------------------------------------------------------------------------

module Choreography.Network where

import Choreography.Location
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

class Backend c where
  runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a
