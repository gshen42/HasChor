--------------------------------------------------------------------------------
-- This module provides the freer monad `Freer`, which allows manipulating
-- effectual computation algebraically.
--------------------------------------------------------------------------------

module Control.Monad.Freer where

import Control.Monad ((>=>))

data Freer f a where
  Pure :: a -> Freer f a
  Do   :: f b -> (b -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Do eff k) = Do eff (fmap f . k)

instance Applicative (Freer f) where
  pure = Pure

  (Pure f)   <*> a = fmap f a
  (Do eff k) <*> a = Do eff $ (<*> a) . k

instance  Monad (Freer f) where
  (Pure a)   >>= f = f a
  (Do eff k) >>= f = Do eff (k >=> f)

runFreer :: (forall a. f a -> a) -> Freer f a -> a
runFreer alg (Pure a)   = a
runFreer alg (Do eff k) = runFreer alg $ k (alg eff)

-- `runFreer` is actually an specialized verison of thie function with
-- `g` being the `Identity` monad
interpFreer :: Monad g => (forall a. f a -> g a) -> Freer f a -> g a
interpFreer f (Pure a)   = return a
interpFreer f (Do eff k) = f eff >>= interpFreer f . k
