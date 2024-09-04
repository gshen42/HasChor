-- | This module defines the `Async` monad, a monad for asynchronous computations.
-- The module intentially exports a minimal set of entities with  the expection
-- that the user will mainly use the `Monad`, `Applicative`, and `MonadIO` 
-- instances of `Async`.
--
-- Do not confuse this module with @Control.Concurrent.Async@ from the @async@
-- package. The aforementioned behaves more like a future, while the `Async`
-- defined here represent computations returnning futures.
--
-- /A note on algebraic laws:/ The various instances of `Async` satisfy the
-- algebraic laws in logical sense but not operational sense. For example,
-- `fmap g (fmap f a)` and `fmap (g . f) a` compute the same result, but the
-- former forks two threads while the latter only forks one. Also, we assume
-- that each computation has no observable side effects to other computations.
module Control.Monad.Async (Async, runAsync) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

-- | An asychronous computation.
newtype Async a = Async {unAsync :: IO (MVar a)}

fork :: IO a -> IO (MVar a)
fork m = do
  fut <- newEmptyMVar
  forkIO $ do
    a <- m
    putMVar fut $! a
  return fut

wait :: MVar a -> IO a
wait = takeMVar

-- | Run an asynchronous computation and wait for its result.
runAsync :: Async a -> IO a
runAsync = wait <=< unAsync

instance Functor Async where
  fmap f m = Async $ do
    fut <- unAsync m
    fork $ do
      a <- wait fut
      return $ f a

instance Applicative Async where
  -- TODO: is this efficient? does it make sense to fork a thread to do pure
  -- computation?
  pure = Async . fork . pure

  f <*> a = Async $ do
    futF <- unAsync f
    futA <- unAsync a
    fork $ do
      f' <- wait futF
      a' <- wait futA
      pure $ f' a'

instance Monad Async where
  m >>= f = Async $ do
    fut <- unAsync m
    fork $ do
      a <- wait fut
      fut' <- unAsync $ f a
      wait fut'

instance MonadIO Async where
  liftIO = Async . fork