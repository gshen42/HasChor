--------------------------------------------------------------------------------
-- This module provides the freer monad `Freer`, which allows manipulating
-- effectual computation algebraically.
--------------------------------------------------------------------------------

module Control.Monad.Freer where

import Control.Monad ((>=>))

data Freer f a where
  Return :: a -> Freer f a
  Do     :: f b -> (b -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Return a) = Return (f a)
  fmap f (Do eff k) = Do eff (fmap f . k)

instance Applicative (Freer f) where
  pure = Return

  (Return f) <*> a = fmap f a
  (Do eff k) <*> a = Do eff $ (<*> a) . k

instance Monad (Freer f) where
  (Return a) >>= f = f a
  (Do eff k) >>= f = Do eff (k >=> f)

toFreer :: f a -> Freer f a
toFreer eff = Do eff Return

runFreer :: Monad g => (forall a. f a -> g a) -> Freer f a -> g a
runFreer f (Return a) = return a
runFreer f (Do eff k) = f eff >>= runFreer f . k
