{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Concurrent.Future
import Control.Monad.Freer
import Data.Kind
import Data.Typeable

-- | Signature of the `Choreo` monad.
data ChoreoSig (ls :: [Loc]) (m :: Type -> Type) (a :: Type) where
  Locally ::
    forall l {ls} {m} {a}.
    (Typeable l, Member l ls) =>
    ((Unwrappable l) => m a) ->
    ChoreoSig ls m (a @ l)
  Comm ::
    forall s r {ls} {m} {a}.
    (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
    ((Unwrappable s) => m a) ->
    ChoreoSig ls m (Future a @ r)
  Cond ::
    forall s {b} {ls} {m} {a}.
    (Typeable s, Member s ls) =>
    ((Unwrappable s) => m a) ->
    (a -> Choreo ls m b) ->
    ChoreoSig ls m b

-- | The monad for choreographies.
type Choreo ls m a = Freer (ChoreoSig ls m) a

-- TODO: is there a way to avoid repeating the type signatures?

locally ::
  forall l {ls} {m} {a}.
  (Typeable l, Member l ls) =>
  ((Unwrappable l) => m a) ->
  Choreo ls m (a @ l)
locally act = perform (Locally act)

comm ::
  forall s r {ls} {m} {a}.
  (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
  ((Unwrappable s) => m a) ->
  Choreo ls m (Future a @ r)
comm act = perform (Comm @s @r act)

cond ::
  forall s {b} {ls} {m} {a}.
  (Typeable s, Member s ls) =>
  ((Unwrappable s) => m a) ->
  (a -> Choreo ls m b) ->
  Choreo ls m b
cond act k = perform (Cond @s act k)

type ChoreoIO ls a = Choreo ls IO a

printChoreo :: Choreo ls m a -> IO a
printChoreo = interp handler
  where
    handler :: ChoreoSig ls m a -> IO a
    handler (Comm @s @r a) = print (reify @s) >> print (reify @r) >> return (error "hhh")

epp :: forall (t :: Loc) {ls} {m} {a}. (Typeable t) => Choreo ls m a -> Network m a
epp = interp handler
  where
    handler :: forall ls m a. ChoreoSig ls m a -> Network m a
    handler (Locally @l a) = undefined
    handler (Comm @s @r a) = undefined
    handler (Cond @s a k) = undefined
