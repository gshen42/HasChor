{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Data.Proxy
import Data.Time
import GHC.TypeLits (KnownSymbol)
import System.Environment

primary :: Proxy "primary"
primary = Proxy

worker1 :: Proxy "worker1"
worker1 = Proxy

worker2 :: Proxy "worker2"
worker2 = Proxy

quicksort :: (KnownSymbol a, KnownSymbol b, KnownSymbol c) => Proxy a -> Proxy b -> Proxy c -> [Int] @ a -> Choreo IO ([Int] @ a)
quicksort a b c lst = do
  isEmpty <- a `locally` \unwrap -> pure (null (unwrap lst))
  cond (a, isEmpty) \case
    True -> do
      a `locally` \_ -> pure []
    False -> do
      smaller <- (a, \unwrap -> let x : xs = unwrap lst in pure [i | i <- xs, i <= x]) ~~> b
      smaller' <- quicksort b c a smaller
      smaller'' <- (b, smaller') ~> a
      bigger <- (a, \unwrap -> let x : xs = unwrap lst in pure [i | i <- xs, i > x]) ~~> c
      bigger' <- quicksort c a b bigger
      bigger'' <- (c, bigger') ~> a
      a `locally` \unwrap -> pure $ unwrap smaller'' ++ [head (unwrap lst)] ++ unwrap bigger''

mainChoreo :: Choreo IO ()
mainChoreo = do
  lst <- primary `locally` \unwrap -> do return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- quicksort primary worker1 worker2 lst
  primary `locally` \unwrap -> do
    print (unwrap sorted)
    return ()
  return ()

main :: IO ()
main = do
  config <- mkLocalConfig locs
  mapConcurrently_ (runChoreography config mainChoreo) locs
  return ()
  where
    locs = ["primary", "worker1", "worker2"]
