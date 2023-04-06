-- | This module defines the freer monad `Freer`, which allows manipulating
-- effectful computations algebraically.
module Control.Monad.Freer where

import Control.Monad ((>=>))

-- | Freer monads.
--
-- A freer monad @Freer f a@ represents an effectful computation that returns a
-- value of type @a@. The parameter @f :: * -> *@ is a effect signature that
-- defines the effectful operations allowed in the computation. @Freer f a@ is
-- called a freer monad in that it's a `Monad` given any @f@.
data Freer f a where
  -- | A pure computation.
  Return :: a -> Freer f a
  -- | An effectufl computation where the first argument @f b@ is the effect
  -- to perform and returns a result of type @b@; the second argument
  -- @b -> Freer f a@ is a continuation that specifies the rest of the
  -- computation given the result of the performed effect.
  Do :: f b -> (b -> Freer f a) -> Freer f a

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

-- | Lift an effect into the freer monad.
toFreer :: f a -> Freer f a
toFreer eff = Do eff Return

-- | Interpret the effects in a freer monad in terms of another monad.
interpFreer :: Monad m => (forall a. f a -> m a) -> Freer f a -> m a
interpFreer handler (Return a) = return a
interpFreer handler (Do eff k) = handler eff >>= interpFreer handler . k
