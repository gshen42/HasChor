module Control.Monad.Tree where

data Tree f a where
  Perf :: f a -> Tree f a
  Pure :: a -> Tree f a
  App :: Tree f (a -> b) -> Tree f a -> Tree f b
  Bind :: Tree f a -> (a -> Tree f b) -> Tree f b

instance Functor (Tree f) where
  fmap f t = Bind t (Pure . f)

instance Applicative (Tree f) where
  pure = Pure
  (<*>) = App

(||) :: Tree f a -> Tree f b -> Tree f ()
t1 || t2 = Pure (\_ _ -> ()) <*> t1 <*> t2

instance Monad (Tree f) where
  (>>=) = Bind

-- Note that `Tree` is just syntax, and it doesn't satisfy the laws of the above three type class
-- instances. For example, `fmap id t` is not definitionally equal to `t`.
-- So why define these instances and how do we know they're safe to use?
-- For the first question, we define these instances to make `Tree` work nicely with existing
-- Haskell ecosystem, especially reusing the do-notation.
-- For the second question, we expect the semantics of `Tree` is given by an interpretation,
-- and each interpretation satisfies the type class laws.

interp :: (Applicative m, Monad m) => (forall a. f a -> m a) -> Tree f a -> m a
interp hdl (Perf e) = hdl e
interp hdl (Pure a) = pure a
interp hdl (App f a) = interp hdl f <*> interp hdl a
interp hdl (Bind t k) = interp hdl t >>= (interp hdl . k)
