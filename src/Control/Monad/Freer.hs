-- | This module defines the freer monad `Freer`, which allows defining
-- computations with interpretable effects.
module Control.Monad.Freer where

import Control.Monad ((>=>))

-- | The freer monad.
--
-- A freer monad @Freer f a@ represents an effectful computation that returns a
-- value of type @a@. The parameter @f :: * -> *@ is a effect signature that
-- defines the effectful operations allowed in the computation. @Freer f a@ is
-- called a freer monad because it's a `Monad` for any given @f@.
data Freer f a where
  -- | A pure computation.
  Return :: a -> Freer f a
  -- | An effectful computation where the first argument @f b@ is the effect
  -- to perform and returns a result of type @b@; the second argument
  -- @b -> Freer f a@ is a continuation that specifies the rest of the
  -- computation given the result of the performed effect.
  Bind :: f b -> (b -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Return a) = Return (f a)
  fmap f (Bind e k) = Bind e (fmap f . k)

instance Applicative (Freer f) where
  pure = Return

  (Return f) <*> a = fmap f a
  (Bind e k) <*> a = Bind e $ (<*> a) . k

instance Monad (Freer f) where
  (Return a) >>= f = f a
  (Bind e k) >>= f = Bind e (k >=> f)

-- | Perform an effect in the freer monad.
perform :: f a -> Freer f a
perform e = Bind e Return

-- | Interpret the effects in a freer monad in terms of another monad.
interp :: (Monad m) => (forall a. f a -> m a) -> Freer f a -> m a
interp handler (Return a) = return a
interp handler (Bind e k) = handler e >>= interp handler . k
