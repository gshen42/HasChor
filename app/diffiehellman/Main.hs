{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http (mkConfig, runNetwork)
import Data.Proxy
import Data.Time
import System.Environment
import System.Random

-- helper functions around prime number
-- https://nulldereference.wordpress.com/2012/02/04/generating-prime-numbers-with-haskell/
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1 : [y | y <- [2 .. (x `div` 2)], x `mod` y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primeNums :: [Integer]
primeNums = [x | x <- [2 ..], isPrime x]

-- set up proxies
alice :: Proxy "alice"
alice = Proxy

bob :: Proxy "bob"
bob = Proxy

diffieHellman :: Choreo IO (Integer @ "alice", Integer @ "bob")
diffieHellman = do
  -- wait for alice to initiate the process
  alice `locallyDo` \unwrap -> do
    putStrLn "enter to start key exchange..."
    getLine
  bob `locallyDo` \unwrap -> do
    putStrLn "waiting for alice to initiate key exchange"

  -- alice picks p and g and sends them to bob
  pa <-
    alice `locallyDo` \unwrap -> do
      x <- randomRIO (200, 1000 :: Int)
      return $ primeNums !! x
  pb <- (alice, pa) ~> bob
  ga <- alice `locallyDo` \unwrap -> do randomRIO (10, unwrap pa)
  gb <- (alice, ga) ~> bob

  -- alice and bob select secrets
  a <- alice `locallyDo` \unwrap -> do randomRIO (200, 1000 :: Integer)
  b <- bob `locallyDo` \unwrap -> do randomRIO (200, 1000 :: Integer)

  -- alice and bob computes numbers that they exchange
  a' <- alice `locallyDo` \unwrap -> do return $ unwrap ga ^ unwrap a `mod` unwrap pa
  b' <- bob `locallyDo` \unwrap -> do return $ unwrap gb ^ unwrap b `mod` unwrap pb

  -- exchange numbers
  a'' <- (alice, a') ~> bob
  b'' <- (bob, b') ~> alice

  -- compute shared key
  s1 <-
    alice `locallyDo` \unwrap ->
      let s = unwrap b'' ^ unwrap a `mod` unwrap pa
       in do
            putStrLn ("alice's shared key: " ++ show s)
            return s
  s2 <-
    bob `locallyDo` \unwrap ->
      let s = unwrap a'' ^ unwrap b `mod` unwrap pb
       in do
            putStrLn ("bob's shared key: " ++ show s)
            return s
  return (s1, s2)

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "alice" -> runNetwork config "alice" aliceP
    "bob" -> runNetwork config "bob" bobP
  return ()
  where
    aliceP = epp diffieHellman "alice"
    bobP = epp diffieHellman "bob"
    config =
      mkConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]
