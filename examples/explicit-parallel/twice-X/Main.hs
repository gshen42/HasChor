{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
-- import Data.List.Singletons
import GHC.TypeLits
import System.Environment
-- import Control.Concurrent (MVar, newMVar, withMVar)
-- import Control.Monad (forever)

$(mkLoc "p")
$(mkLoc "q")
$(mkLoc "r1")
$(mkLoc "r2")

-- Ozone: Figure 5
twiceX :: Choreo IO ()
twiceX = do
  -- locks <- mapM makeLock [p, q, r1, r2]
  x p r1 q `par` x p r2 q
  where
    x :: (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
      Proxy a -> Proxy b -> Proxy c -> Choreo IO ()
    x a b c = do
      aw <- a `locally` (\_ -> produce)
      bx <- aw ~> b
      cy <- (b, \un -> transform (un bx)) ~~> c
      az <- (c, \un -> process (un cy)) ~~> a
      a `locally` (\un -> store (un aw) (un az))
      return ()

    -- This behaves poorly without a lock if multiple threads are on the same
    -- endpoint; both putStrLn run before any input is given
    produce :: IO Int
    produce = putStrLn "Enter an integer: " >> read <$> getLine

    transform :: Int -> IO Int
    transform = return <$> (\x -> x*x)

    process :: Int -> IO Int
    process = return <$> (+ 5)

    store :: Int -> Int -> IO ()
    store x y = putStrLn ("Storing \"" ++ show x ++ "\" and \"" ++ show y ++ "\"")

    -- makeLock :: KnownSymbol l => Proxy l -> Choreo IO (MVar () @ l)
    -- makeLock =
    --   _


main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg twiceX loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("p", ("localhost", 4242))
      , ("q", ("localhost", 4343))
      , ("r1", ("localhost", 4444))
      , ("r2", ("localhost", 4445))
      ]
