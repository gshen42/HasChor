{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for choreographic programming.
module Choreography.Choreo where

import Choreography.Located
import Choreography.Location
import Choreography.Network hiding (Sig)
import Control.Monad.Async
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.HashMap.Strict as HM

-- * The Choreo monad

-- | A different notation that might be more readable: `a @ l` is an asynchronous
-- computation of type `a` and located at `l`.
type a @ l = Located l Async a

-- | Effect signature for the `Choreo` monad.
data Sig a where
  Comm ::
    (Show a, Read a) =>
    Loc s ->
    Loc r ->
    Located s Async a ->
    Sig (Located r Async a)
  Cond ::
    (Show a, Read a) =>
    Loc s ->
    Located s Async a ->
    (a -> Choreo b) ->
    Sig b
  -- a variant of `Comm` where the sender and receiver are the same and the
  -- result does not need to be serializable
  Local ::
    Loc l ->
    Located l Async a ->
    Sig (Located l Async a)

-- | The monad for choreographies.
newtype Choreo a = Choreo {unChoreo :: Freer Sig a}
  deriving (Functor, Applicative, Monad)

-- | Communication between a sender and a receiver.
comm ::
  (Show a, Read a) =>
  -- | The sender
  Loc s ->
  -- | The receiver
  Loc r ->
  -- | A located computation at sender
  Located s Async a ->
  Choreo (Located r Async a)
comm s r a = Choreo $ perform $ Comm s r a

-- | Branch on the result of a located computation.
cond ::
  (Show a, Read a) =>
  -- | The sender
  Loc s ->
  -- | The scrutinee
  Located s Async a ->
  -- | A function that describes the follow-up choreographies based on the
  -- value of scrutinee.
  (a -> Choreo b) ->
  Choreo b
cond s a f = Choreo $ perform $ Cond s a f

-- | Perform a local computation at a given location.
locally ::
  -- | Location performing the local computation.
  Loc l ->
  -- | The local computation given a constrained unwrap funciton.
  Located l Async a ->
  Choreo (Located l Async a)
locally l a = Choreo $ perform $ Local l a

-- * Endpoint projection

-- type SeqIdMap = HashMap LocTm SeqId

-- newSeqId :: (Monad m) => Loc l -> StateT SeqIdMap m SeqId
-- newSeqId l = do
--   m <- get
--   let l' = toLocTm l
--   let v = HM.findWithDefault 0 l' m
--   modify $ insert l' (v + 1)
--   return v

-- handler :: (MonadIO m) => Loc t -> ChoreoSig fut m a -> StateT SeqIdMap (Network fut m) a
-- handler t (Comm s r a) = undefined

-- sender and receiver are the same
-- \| eqLoc s r = do
--     v <- lift $ lift $ unwrap a
--     return (present v)
--   -- target is the sender
--   | eqLoc t s = do
--       i <- newSeqId s
--       -- TODO: why the double lifts?
--       v <- lift $ lift $ _ a
--       lift $ send (v, s, i) r
--       return absent
--   -- target is the receiver
--   | eqLoc t r = do
--       i <- newSeqId s
--       v <- lift $ recv (s, i)
--       return (present v)
--   -- target is neither the sender nor the receiver
--   | otherwise = return absent
-- handler t (Cond s a f)
--   -- target is the sender
--   | eqLoc t s = do
--       i <- newSeqId s
--       v <- lift $ lift $ _ a
--       lift $ broadcast (v, s, i)
--       lift $ epp (f v) t
--   -- target is the receiver
--   | otherwise = do
--       i <- newSeqId s
--       v <- lift $ _ -- recv (s, i)
--       -- select messages are synchronous
--       v' <- liftIO $ _
--       lift $ epp (f v') t
-- handler t (Local l a)
--   | eqLoc t l = do
--       v <- lift $ lift $ _ a
--       return (present v)
--   | otherwise = return absent

epp :: Choreo a -> Loc l -> Network a
epp c t = undefined -- evalStateT (interp (handler t) (unChoreo c)) HM.empty
