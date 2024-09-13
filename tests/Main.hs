{-# LANGUAGE DataKinds #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Control.Concurrent.Async

data Client1

data Client2

data Server

ex :: ChoreoIO [Server, Client1, Client2] ()
ex = do
  input1 <- comm @Client1 @Server $ getLine
  input2 <- comm @Client2 @Server $ return "str"

  commFork @Server @Client1 $ do
    x <- wait (un input1)
    return (x ++ "hahaha")
  commFork @Server @Client2 $ do
    x <- wait (un input2)
    return (x ++ "hahaha")

  return ()

main :: IO ()
main = putStrLn "hello"
