{-# LANGUAGE ImpredicativeTypes #-}

module Choreography.Choreo where

import Choreography.Network
import Control.Monad.Tree
import Data.Binary
import Data.Kind (Type)
import GHC.TypeLits.Singletons

-- This way of representing located values is type-unsafe, that is, the type system doesn't
-- guarantee that the unwrap function is always applied to a located value that's a `Just`, and the
-- implementation needs to maintain the invariant that for location `l`, `a @ l` is always a `Just`.
-- TODO: Can we make located values type-safe? Using a two-step endpoint projection? That might be
-- out of the scope of the project.
newtype (a :: Type) @ (l :: Symbol) = At { unAt :: Maybe a }

type Unwrap l = forall a. a @ l -> a

unwrapUnsafe :: a @ l -> a
unwrapUnsafe (At (Just a)) = a
unwrapUnsafe (At Nothing) = error "HasChor Internal Error: unwrapping an empty located value."

data ChoreoSig m a where
  Locally :: (KnownSymbol l) => SSymbol l -> (Unwrap l -> m a) -> ChoreoSig m (a @ l)
  Comm :: (Binary a, KnownSymbol l, KnownSymbol l') =>  a @ l -> SSymbol l' -> ChoreoSig m (a @ l')
  Cond :: (Binary a, KnownSymbol l) => a @ l -> (a -> Choreo m b) -> ChoreoSig m b

type Choreo m a = Tree (ChoreoSig m) a

locally :: (KnownSymbol l) => SSymbol l -> (Unwrap l -> m a) -> Choreo m (a @ l)
locally l a = Perf (Locally l a)

(~>) :: (Binary a, KnownSymbol l, KnownSymbol l') => a @ l -> SSymbol l' -> Choreo m (a @ l')
a ~> l' = Perf (Comm a l')

-- TODO: figure out why `Unwrap l -> m a` requires `ImpredicativeTypes` and is this safe
(~~>) :: (Binary a, KnownSymbol l, KnownSymbol l') =>
  (SSymbol l, Unwrap l -> m a) -> SSymbol l' -> Choreo m (a @ l')
(l, a) ~~> l' = do
  x <- l `locally` a
  x ~> l'

cond :: (Binary a, KnownSymbol l) => a @ l -> (a -> Choreo m b) -> Choreo m b
cond a f = Perf (Cond a f)

epp :: Choreo m a -> SSymbol l -> Network m a
epp c t = interp hdl c
  where
    hdl :: ChoreoSig m a -> Network m a
    hdl (Locally l m) = _
    hdl (Comm a r) = _
    hdl (Cond a b) = _
    
