-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module Choreography.Network where

import Choreography.Location (LocTm)
import Control.Concurrent.Async (Async)
import Control.Monad.Freer (Freer, perform)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))

-- | An id that uniquely identifies messages from the same sender.
type SeqNum = Int

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  Lift :: m a -> NetworkSig m a
  Send :: (Show a) => a -> LocTm -> SeqNum -> NetworkSig m (Async ())
  Recv :: (Read a) => LocTm -> SeqNum -> NetworkSig m (Async a)

-- | The monad for network programs.
newtype Network m a = Network {unNetwork :: Freer (NetworkSig m) a}
  deriving (Functor, Applicative, Monad)

-- | Perform an operation in the local monad.
instance MonadTrans Network where
  lift = Network . perform . Lift

-- | Perform a IO operation if the local monad subsumes IO.
instance (MonadIO m) => MonadIO (Network m) where
  liftIO = lift . liftIO

-- | Send a message to a receiver.
send :: (Show a) => a -> LocTm -> SeqNum -> Network m (Async ())
send a r i = Network $ perform $ Send a r i

-- | Receive a message from a sender.
recv :: (Read a) => LocTm -> SeqNum -> Network m (Async a)
recv s i = Network $ perform $ Recv s i

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> LocTm -> Network m a -> m a
