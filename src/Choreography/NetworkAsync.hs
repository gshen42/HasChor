-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module Choreography.NetworkAsync where

import Choreography.Location (LocTm)
import Control.Concurrent.Async (Async)
import Control.Monad.Freer (Freer, toFreer)
import Control.Monad.IO.Class (MonadIO)

-- * The Network monad

-- | An id that uniquely identifies messages from the same sender.
type SeqId = Int

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  Run :: m a -> NetworkSig m a
  Send :: (Show a) => a -> LocTm -> SeqId -> NetworkSig m (Async ())
  Recv :: (Read a) => LocTm -> SeqId -> NetworkSig m (Async a)
  BCast :: (Show a) => a -> SeqId -> NetworkSig m [Async ()]

-- | Monad that represents network programs
type Network m = Freer (NetworkSig m)

-- | Perform a local compuation.
run :: m a -> Network m a
run m = toFreer $ Run m

-- | Send a message to a receiver.
send :: (Show a) => a -> LocTm -> SeqId -> Network m (Async ())
send a l id = toFreer $ Send a l id

-- | Receive a message from a sender.
recv :: (Read a) => LocTm -> SeqId -> Network m (Async a)
recv l id = toFreer $ Recv l id

-- | Broadcast a message to all participants.
broadcast :: (Show a) => a -> SeqId -> Network m [Async ()]
broadcast a id = toFreer $ BCast a id

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> LocTm -> Network m a -> m a
