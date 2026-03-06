{-# LANGUAGE ImpredicativeTypes #-}

module Choreography.Choreo where

import Control.Monad.Tree
import Data.Binary
import Data.Kind (Type)
import Data.Singletons.Decide ((:~:), (%~), Decision(..))
import GHC.TypeLits.Singletons

newtype (a :: Type) @ (l :: Symbol) = At { unAt :: forall t. (t :~: l) -> a }

type Unwrap l = forall a. a @ l -> a

mkUnwrap :: (t :~: l) -> Unwrap l
mkUnwrap pf (At x) = x pf

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
