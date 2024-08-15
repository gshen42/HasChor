{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Concurrent.Async
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict as HM
import Data.Maybe

-- * Located computation

-- | A located computation. @At l m a@ denotes a computation lcoated at @l@
-- within the monad @m@ and returns a value of type @a@. It is semantically
-- equivalent to @m (Maybe a)@.
newtype At l m a = At {unAt :: MaybeT m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | Unwrap a located value.
--
-- /Note:/ Unwrapping a empty located value will throw an exception.
unwrap :: (Monad m) => At l m a -> m a
unwrap = fmap (fromMaybe $ error "internal error") . runMaybeT . unAt

absent :: (Monad m) => At l m a
absent = At $ MaybeT $ return Nothing

present :: (Monad m) => a -> At l m a
present = At . MaybeT . return . Just

-- * The Choreo monad

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
data ChoreoSig m a where
  Comm ::
    (Show a, Read a) =>
    Loc s ->
    Loc r ->
    At s m a ->
    ChoreoSig m (At r m (Async a))
  Cond ::
    (Show a, Read a) =>
    Loc s ->
    At s m a ->
    (a -> Choreo m b) ->
    ChoreoSig m b
  Local ::
    Loc l ->
    At l m a ->
    ChoreoSig m (At l m a)

-- | Monad for writing choreographies.
newtype Choreo m a = Choreo {unChoreo :: Freer (ChoreoSig m) a}
  deriving (Functor, Applicative, Monad)

-- | Communication between a sender and a receiver.
comm ::
  (Show a, Read a) =>
  -- | A pair of sender and receiver
  (Loc s, Loc r) ->
  -- | A located computation at sender
  At s m a ->
  Choreo m (At r m (Async a))
comm (s, r) a = Choreo $ toFreer (Comm s r a)

-- | Branch on the result of a located computation.
cond ::
  (Show a, Read a) =>
  -- | A pair of a location and a scrutinee located on it.
  (Loc s, At s m a) ->
  -- | A function that describes the follow-up choreographies based on the
  -- value of scrutinee.
  (a -> Choreo m b) ->
  Choreo m b
cond (s, a) f = Choreo $ toFreer (Cond s a f)

-- | Perform a local computation at a given location.
locally ::
  -- | Location performing the local computation.
  Loc l ->
  -- | The local computation given a constrained unwrap funciton.
  At l m a ->
  Choreo m (At l m a)
locally l a = Choreo $ toFreer (Local l a)

-- * Endpoint projection

type SeqIdMap = HashMap LocTm SeqId

epp :: (MonadIO m) => Choreo m a -> Loc l -> Network m a
epp c t = evalStateT (interpFreer handler (unChoreo c)) HM.empty
  where
    handler :: (MonadIO m) => ChoreoSig m a -> StateT SeqIdMap (Network m) a
    handler (Comm s r a)
      -- sender and receiver are the same
      | eqLoc s r = undefined
      -- target is the sender
      | eqLoc t s = do
          i <- newSeqId s
          -- TODO: why the double lifts?
          v <- lift $ lift $ unwrap a
          lift $ send (v, s, i) r
          return absent
      -- target is the receiver
      | eqLoc t r = do
          i <- newSeqId s
          v <- lift $ recv (s, i)
          return (present v)
      -- target is neither the sender nor the receiver
      | otherwise = return absent
    handler (Cond s a f)
      -- target is the sender
      | eqLoc t s = do
          i <- newSeqId s
          v <- lift $ lift $ unwrap a
          lift $ broadcast (v, s, i)
          lift $ epp (f v) t
      -- target is the receiver
      | otherwise = do
          i <- newSeqId s
          v <- lift $ recv (s, i)
          -- select messages are synchronous
          v' <- liftIO $ wait v
          lift $ epp (f v') t
    handler (Local l a)
      | eqLoc t l = do
          v <- lift $ lift $ unwrap a
          return (present v)
      | otherwise = return absent

    newSeqId :: (Monad m) => Loc l -> StateT SeqIdMap m SeqId
    newSeqId l = do
      m <- get
      let l' = toLocTm l
      let v = HM.findWithDefault 0 l' m
      modify (insert l' (v + 1))
      return v
