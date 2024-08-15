-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module Choreography.Network where

import Choreography.Location
import Control.Concurrent.Async
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- * The Network monad

-- | An id that uniquely identifies messages from the same sender.
type SeqId = Int

-- | An id that uniquely identifies messages across different senders.
type MsgId = (LocTm, SeqId)

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  Run :: m a -> NetworkSig m a
  Send :: (Show a) => a -> MsgId -> LocTm -> NetworkSig m (Async ())
  Recv :: (Read a) => MsgId -> NetworkSig m (Async a)
  BCast :: (Show a) => a -> MsgId -> NetworkSig m [Async ()]

-- | Monad that represents network programs
newtype Network m a = Network {unNetwork :: Freer (NetworkSig m) a}
  deriving (Functor, Applicative, Monad)

-- | Perform a local computation.
instance MonadTrans Network where
  lift = Network . toFreer . Run

-- | Send a message to a receiver.
send :: (Show a) => (a, Loc s, SeqId) -> Loc r -> Network m (Async ())
send (a, s, i) r = Network $ toFreer $ Send a (toLocTm s, i) (toLocTm r)

-- | Receive a message from a sender.
recv :: (Read a) => (Loc s, SeqId) -> Network m (Async a)
recv (s, i) = Network $ toFreer $ Recv (toLocTm s, i)

-- | Broadcast a message to all participants.
broadcast :: (Show a) => (a, Loc s, SeqId) -> Network m [Async ()]
broadcast (a, s, i) = Network $ toFreer $ BCast a (toLocTm s, i)

-- | Perform a IO operation if the local monad subsumes IO.
instance (MonadIO m) => MonadIO (Network m) where
  liftIO = Network . toFreer . Run . liftIO

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> LocTm -> Network m a -> m a
