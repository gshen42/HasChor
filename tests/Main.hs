{-# LANGUAGE DataKinds #-}

module Main where

import Choreography.Choreo
import Choreography.Located
import Choreography.Location
import Control.Concurrent.Async
import Control.Monad.IO.Class

-- Semi-deterministic operations:
--
-- data Async a -- futures
-- wait :: (MonadIO m) => Async a -> m a
-- waitUntil :: Async a -> Int -> IO (Maybe a)
-- waitAny :: [Async a] -> IO a
-- waitQuorum :: [Async Bool] -> Bool
--
-- locally :: forall l. ((Unwrappable l) => m a) -> Choreo m a
-- comm :: forall s r. ((Unwrappable s) => m a) -> Choreo m (Async a @ r)
-- cond :: forall s. ((Unwrapaable s) => m a) -> (a -> Choreo m b) -> Choreo m b
--
-- 1. only difference from the old APIs (aside from some cosmetic changes): `comm` returns a `Async` at the receiver
-- 2. the programming model is semi-deterministic (given the local programs are deterministic and don't use `waitUntil` or `waitAny` --- don't change behavior based on message arrival order?)
-- 3. the non-deterministic behavarios are either seprated at different locaitons or compensated by sequence numbers
-- 4. all these operations happen in order
--
-- Non-deterministic operations:
--
-- locallyFork :: forall l. ((Unwrappable l) => m a) -> Choreo m (Async a)
-- commFork :: forall s r. ((Unwrappable s) => m a) -> Choreo m (Async a @ r)
--
-- 1. same semantics as locally/comm except spawning a new thread and performing the action in that thread
-- 2. introduce concurrency at each location
-- 3. these operations happen out of order

data Client1

data Client2

data Server

ex :: ChoreoIO [Server, Client1, Client2] ()
ex = do
  input1 <- comm @Client1 @Server $ liftIO getLine
  input2 <- comm @Client2 @Server $ return "str"

  commFork @Server @Client1 $ do
    input1 <- unwrap input1
    x <- liftIO $ wait input1
    return (x ++ "hahaha")
  commFork @Server @Client2 $ do
    input2 <- unwrap input2
    x <- liftIO $ wait input2
    return (x ++ "hahaha")

  return ()

main :: IO ()
main = putStrLn "hello"
