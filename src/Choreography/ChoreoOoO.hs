{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | The `Choreo` monad with out-of-order semantics.
module Choreography.ChoreoOoO where

import Choreography.Located
import Choreography.Location
import Choreography.Network hiding (Sig)
import Control.Monad.Async
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.HashMap.Strict as HM

-- | A different notation that might be more readable: `a @ l` is an asynchronous
-- computation of type `a` and located at `l`.
type a @ l = Located l Async a

-- | Effect signature for the `Choreo` monad.
data ChoreoSig a where
  Comm ::
    (Show a, Read a) =>
    Loc s ->
    Loc r ->
    Located s Async a ->
    ChoreoSig (Located r Async a)
  Cond ::
    (Show a, Read a) =>
    Loc s ->
    Located s Async a ->
    (a -> Choreo b) ->
    ChoreoSig b
  -- a variant of `Comm` where the sender and receiver are the same and the
  -- result does not need to be serializable
  Local ::
    Loc l ->
    Located l Async a ->
    ChoreoSig (Located l Async a)

-- | The monad for choreographies.
newtype Choreo a = Choreo {unChoreo :: Freer ChoreoSig a}
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

-- | The state of `epp`.
data EppState = EppState
  { seqIdMap :: HashMap LocTm SeqId,
    -- TODO: Use a heterogeneous list so that we don't need to create dummy
    -- `Async`s
    asyncList :: [Async ()]
  }

-- | Get the next sequence ID for a location (return 0 if there's none) and set
-- the next sequence ID to the current value plus 1.
newSeqId :: (MonadState EppState m) => Loc l -> m SeqId
newSeqId l = do
  s <- get
  let l' = toLocTm l
  let v = HM.findWithDefault 0 l' (seqIdMap s)
  put $ s {seqIdMap = HM.insert l' (v + 1) (seqIdMap s)}
  return v

-- | Convert any `Async a` to `Async ()`.
dummy :: Async a -> Async ()
dummy = fmap (const ())

-- | Add an `Async` to the list of `Async`s.
addAsync :: (MonadState EppState m) => Async a -> m ()
addAsync a = modify $ \s -> s {asyncList = dummy a : asyncList s}

epp1 :: Choreo a -> Loc t -> StateT EppState Identity (Network a)
epp1 = undefined

epp :: Choreo a -> Loc t -> Network a
epp c t =
  let (n, s) = runIdentity $ runStateT (epp1 c t) (EppState HM.empty [])
   in n >>= \a -> liftIO (runAsync $ sequenceA $ asyncList s) >> return a

-- epp :: Choreo a -> Loc t -> Network a
-- epp c t = evalStateT (interp (handler t) (unChoreo c)) HM.empty
--   where
--     handler :: Loc t -> Sig a -> StateT EppState Network a
--     handler t (Comm s r a)
--       | eqLoc s r = undefined
--       | eqLoc t s = undefined
--       | eqLoc t r = undefined
--       | otherwise = undefined
--     handler t (Cond s a f)
--       | eqLoc t s = undefined
--       | otherwise = undefined
--     handler t (Local l a)
--       | eqLoc t l = undefined
--       | otherwise = undefined
