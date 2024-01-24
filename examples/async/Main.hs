module Main where

import Control.Concurrent.Async
import Choreography.NetworkAsync
import Choreography.NetworkAsync.Http
import System.Environment

----------------------------------------------------------------------
-- Test asynchronous network programs

progA :: Network IO ()
progA = do
  lift $ getLine
  send () "D" 1000
  return ()

progB :: Network IO ()
progB = do
    lift $ getLine
    send () "D" 1000
    return ()

progC :: Network IO ()
progC = do
    lift $ getLine
    send () "D" 1000
    return ()

progD :: Network IO ()
progD = do
  (a :: Async ()) <- recv "A" 1000
  (b :: Async ()) <- recv "B" 1000
  (c :: Async ()) <- recv "C" 1000
  lift $ putStrLn "I can do something else while waiting!"
  (x, _) <- lift $ waitAny [a, b, c]
  if x == a
    then lift $ putStrLn "A comes first"
    else if x == b
         then lift $ putStrLn "B comes first"
         else lift $ putStrLn "C comes first"

test1 :: IO ()
test1 = do
  [loc] <- getArgs
  case loc of
    "A" -> runNetwork cfg "A" progA
    "B" -> runNetwork cfg "B" progB
    "C" -> runNetwork cfg "C" progC
    "D" -> runNetwork cfg "D" progD
  return ()
  where
    cfg = mkHttpConfig [ ("A", ("localhost", 4242))
                       , ("B", ("localhost", 4343))
                       , ("C", ("localhost", 4444))
                       , ("D", ("localhost", 4545))
                       ]

----------------------------------------------------------------------
-- Test asynchronous choreographies

test2 :: IO ()
test2 = undefined

----------------------------------------------------------------------
-- Entry point

main = test1
-- main = test2
