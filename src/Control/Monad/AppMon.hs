module Control.Monad.AppMon where

data AppMon e a where
  Perf :: e a -> AppMon e a
  Pure :: a -> AppMon e a
  App  :: AppMon e (a -> b) -> AppMon e a -> AppMon e b
  Bind :: AppMon e a -> (a -> AppMon e b) -> AppMon e b

instance Functor (AppMon e) where
  fmap f m = App (Pure f) m

instance Applicative (AppMon a) where
  pure  = Pure
  (<*>) = App

instance Monad (AppMon a) where
  (>>=) = Bind

(||) :: (Applicative f) => f a -> f b -> f (a, b)
x || y = (,) <$> x <*> y

interp :: (Applicative m, Monad m)
       => (forall a. e a -> m a)
       -> AppMon e a
       -> m a
interp h (Perf e)   = h e
interp h (Pure a)   = pure a
interp h (App p q)  = (interp h p) <*> (interp h q)
interp h (Bind p k) = (interp h p) >>= (interp h . k)

