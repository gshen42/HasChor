{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Concurrent.Async
import Choreography.Location
import Choreography.NetworkAsync
import Choreography.NetworkAsync.Http
import Choreography.ChoreoAsync
import System.Environment
import Data.Proxy
import Control.Monad

-- 1. `Async a` denotes a asynchornous computation that returns a
-- value of type `a`. There's a set of functions to work with `Async`,
-- like `wait`, `waitAny`.

-- 2. Asynchrony is completely done at the local language level with
-- comm (~>) returnning a Async. `wait` is a local langauge function.

-- 3. Send and recv in the network programs are explicitly marked with
-- a sequence id to connect received messages to `Async`s. This is
-- very similar to the counter in our operational semantics except
-- it's statically calculated.
--
-- Example:
--
-- A:                             B:
--   send "hello" B                 x <- recv A
--   send "world" B                 y <- recv A
--                                  print (x ++ y)
--
-- Question: What should B print?
--
--
-- A:                             B:
--   send "hello" B 1000            x <- recv A 1000
--   send "world" B 1001            y <- recv A 1001
--                                  print (x ++ y)
--
-- B should print "helloworld"

-- 4. `epp` is responsible for generating sends and receives with
-- mathching sequence numbers.

-- 5. The HTTP backend uses a message buffer to store received
-- messages, which is a map from (LocTm, SeqId) to messages.

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

locA :: Proxy "A"
locA = Proxy

locB :: Proxy "B"
locB = Proxy

locC :: Proxy "C"
locC = Proxy

locD :: Proxy "D"
locD = Proxy

choreo :: Choreo IO (() @ "D")
choreo = do
  locA `locally` \_ -> getLine
  a  <- (locA, wrap ()) ~> locD

  locB `locally` \_ -> getLine
  b  <- (locB, wrap ()) ~> locD

  locC `locally` \_ -> getLine
  c  <- (locC, wrap ()) ~> locD

  locD `locally` \un -> putStrLn "I can do something else while waiting!"

  locD `locally` \un -> do
    (x, _) <- waitAny [un a, un b, un c]
    if x == un a
    then putStrLn "A comes first"
    else if x == un b
         then putStrLn "B comes first"
         else putStrLn "C comes first"

test2 :: IO ()
test2 = do
  [loc] <- getArgs
  void $ runChoreography cfg choreo loc
  where
    cfg = mkHttpConfig [ ("A", ("localhost", 4242))
                       , ("B", ("localhost", 4343))
                       , ("C", ("localhost", 4444))
                       , ("D", ("localhost", 4545))
                       ]

----------------------------------------------------------------------
-- Test sequence ids

choreo2 :: Choreo IO (() @ "D")
choreo2 = do
  locA `locally` \_ -> getLine

  a1  <- (locA, wrap "Hello, ") ~> locD
  a2  <- (locA, wrap "world. ") ~> locD
  a3  <- (locA, wrap "I like ") ~> locD
  a4  <- (locA, wrap "choreographic programming.") ~> locD

  locD `locally` \un -> do
    as <- mapM (wait . un) [a1, a2, a3, a4]
    putStrLn (foldr (++) [] as)

test3 :: IO ()
test3 = do
  [loc] <- getArgs
  void $ runChoreography cfg choreo2 loc
  where
    cfg = mkHttpConfig [ ("A", ("localhost", 4242))
                       , ("B", ("localhost", 4343))
                       , ("C", ("localhost", 4444))
                       , ("D", ("localhost", 4545))
                       ]

client :: Proxy "client"
client = Proxy

bank1 :: Proxy "bank1"
bank1 = Proxy

bank2 :: Proxy "bank2"
bank2 = Proxy

choreo4 :: Choreo IO ((Async Int) @ "client")
choreo4 = do
  bal1 <- bank1 `locally` \_ -> read <$> getLine
  bal2 <- bank2 `locally` \_ -> read <$> getLine

  bal1' <- (bank1, bal1) ~> client
  bal2' <- (bank2, bal2) ~> client

  client `locally` \_ -> getLine

  largeAv <- client `locally` \un -> do
    bal1'' <- poll (un bal1')
    bal2'' <- poll (un bal2')
    case (bal1'', bal2'') of
      (Just (Right x), Just (Right y)) -> async (return $ max x y)
      _ -> async (return (-1))
  availBal <- select client bal1' bal2'
  largestBal <- select client largeAv availBal
  client `locally` \un -> do
    a <- wait (un largestBal)
    putStrLn (show a)
  return largestBal

test4 :: IO ()
test4 = do
  [loc] <- getArgs
  void $ runChoreography cfg choreo4 loc
  where
    cfg = mkHttpConfig [ ("client", ("localhost", 4242))
                       , ("bank1", ("localhost", 4343))
                       , ("bank2", ("localhost", 4444))
                       ]

----------------------------------------------------------------------
-- Entry point

-- main = test1
-- main = test2
-- main = test4
main = test4
