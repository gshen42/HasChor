-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module Choreography.Network where

import Choreography.Location
import Control.Monad.Async
import Control.Monad.Freer
import Control.Monad.IO.Class

-- * The Network monad

-- | An id that uniquely identifies messages from the same sender.
type SeqId = Int

-- | Effect NetworkSigature for the `Network` monad.
data Sig a where
  -- Local computation
  LiftIO :: IO a -> Sig a
  -- Communication
  Send :: (Show a) => a -> LocTm -> SeqId -> Sig (Async ())
  Recv :: (Read a) => LocTm -> SeqId -> Sig (Async a)
  -- Knowledge of choice
  BCast :: (Show a) => a -> SeqId -> Sig (Async [()])

-- | Network programs.
newtype Network a = Network {unNetwork :: Freer Sig a}
  deriving (Functor, Applicative, Monad)

-- | Perform a IO operation if the local monad subsumes IO.
instance MonadIO Network where
  liftIO = Network . perform . LiftIO

-- | Send a message to a receiver.
send :: (Show a) => a -> Loc r -> SeqId -> Network (Async ())
send a r i = Network $ perform $ Send a (toLocTm r) i

-- | Receive a message from a sender.
recv :: (Read a) => Loc s -> SeqId -> Network (Async a)
recv s i = Network $ perform $ Recv (toLocTm s) i

-- | Broadcast a message to all participants.
broadcast :: (Show a) => a -> SeqId -> Network (Async [()])
broadcast a i = Network $ perform $ BCast a i

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: c -> LocTm -> Network a -> IO a
