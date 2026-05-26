{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- A type safe version of the Choreo monad, at the cost of adding one more
-- layer of abstraction. If things work out, this version would be
-- API-compabible with the original.
module Choreography.Choreo2 where

import Choreography.Network
import Control.Monad.Reader
import Control.Monad.Tree
import Data.Void
import Data.Singletons.Decide
import Data.Type.Equality
import GHC.TypeLits.Singletons

-- Motivation
--
-- The primary reason for `a @ l` being not type safe in the original `Choreo`
-- monad is that it has a *uniform* representation for any projection location.
-- That is, `a @ l` is represented as a `Maybe`, which, at run time, is
-- impossible to tell if it's a `Just` or `Nothing`.
--
-- The core idea of this version is to project located values at compile time
-- and give each projection location its own version of located values. To make
-- located values projectable, we define them as `At a l p`. Intuitively, a `At
-- a l p` represent an original `a @ l` located value. The additional `p` is
-- the location later we want to project this value to. (From a design
-- perspective, it's probably ideal to hide `p` to a later phase, but I don't
-- know how to do that. Or we can hide it from the user, but I don't know how to
-- do that either. Nevertheless, presenting it to the user doesn't impact
-- usability as they can just ignore that.)

-- A `At a l p` is a value of type `a` if `l` is equal to `p` otherwise empty.
-- Here, empty means that we have nothing to provide and consume, which is
-- essential to defining the type-safe EPP.

newtype At a (l :: Symbol) (p :: Symbol) = At { unAt :: (l :~: p) -> a }

-- Question: is it possible to hide the `p` argument with existential types?
-- Answer: Not really. Because we want all located values in a choreography to share the same `p`
-- and be potentially instantiated to a same location. Using existential types to hide the `p`
-- would make this impossible :-(

-- Now we can define a type-safe unwrap function, which also needs to take a
-- projection location.

type Unwrap l p = forall a. At a l p -> a

unwrap :: (l :~: p) -> Unwrap l p
unwrap pf x = unAt x pf

-- Similarily, we also add projection location to `ChoreoSig` and `Choreo`.

data ChoreoSig m p a where
  Locally :: (KnownSymbol l) =>
    SSymbol l -> (Unwrap l p -> m a) -> ChoreoSig m p (At a l p)
  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
    SSymbol l -> At a l p -> SSymbol l' -> ChoreoSig m p (At a l' p)
  Cond :: (Show a, Read a, KnownSymbol l) =>
    SSymbol l -> At a l p -> (a -> Choreo m p b) -> ChoreoSig m p b

type Choreo m p a = Tree (ChoreoSig m p) a

-- Now, let's define a type-safe EPP function. The `Epp` type is for handling
-- parallel choreographies and is the same as before.

newtype Epp m a = Epp { unEpp :: ReaderT SessionId (Network m) a }
  deriving (Functor, Monad, MonadReader SessionId)

instance Applicative (Epp m) where
  pure = Epp . pure
  f <*> a = Epp $
    local (Nest (Left ())) (unEpp f) <*> local (Nest (Right ())) (unEpp a)

instance MonadTrans Epp where
  lift = Epp . lift . exec

-- Finally, the EPP function.

epp :: forall m p a. Choreo m p a -> SSymbol p -> Epp m a
epp c p = interp handler c
  where
    handler :: forall a. ChoreoSig m p a -> Epp m a
    handler (Locally l m) = case l %~ p of
      (Proved pf) -> Epp $ do
        a <- lift (exec (m (unwrap pf)))
        return (At $ const a)
      (Disproved dpf) -> return (At $ \pf -> absurd (dpf pf))
    handler (Comm s a r) = case s %~ r of
      (Proved Refl) -> return a
      (Disproved dpf) -> case (s %~ p, r %~ p) of
        (Proved pf1, Proved pf2) -> absurd (dpf (trans pf1 (sym pf2)))
        (Proved pf1, Disproved dpf2) -> Epp $ do
          sid <- ask
          lift (send sid (unAt a pf1) (toLocTm r))
          return (At $ \pf -> absurd (dpf2 pf))
        (Disproved dpf1, Proved pf2) -> Epp $ do
          sid <- ask
          a <- lift (recv sid (toLocTm s))
          return (At $ \_ -> a)
        (Disproved dpf1, Disproved dpf2) -> return (At $ \pf -> absurd (dpf2 pf))
    handler (Cond l a k) = case l %~ p of
      (Proved pf) -> let a' = unAt a pf in do
        Epp (do sid <- ask; lift $ broadcast sid a')
        epp (k a') p
      (Disproved dpf) -> do
        x <- Epp (do sid <- ask; lift $ recv sid (toLocTm l))
        epp (k x) p

toLocTm :: (KnownSymbol l) => SSymbol l -> String
toLocTm = symbolVal

-- User-facing operations

-- | Perform a local computation at a given location.
locally :: (KnownSymbol l) => SSymbol l -> (Unwrap l p -> m a) -> Choreo m p (At a l p)
locally l a = Perf (Locally l a)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
  At a l p -> SSymbol l' -> Choreo m p (At a l' p)
a ~> p = Perf (Comm SSymbol a p)

-- TODO: figure out why `Unwrap l -> m a` requires `ImpredicativeTypes` and is this safe
-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
  (SSymbol l, Unwrap l p -> m a) -> SSymbol l' -> Choreo m p (At a l' p)
(l, a) ~~> p = do
  x <- l `locally` a
  x ~> p

-- | Conditionally execute choreographies based on a located value.
cond :: (Show a, Read a, KnownSymbol l) => At a l p -> (a -> Choreo m p b) -> Choreo m p b
cond a f = Perf (Cond SSymbol a f)
