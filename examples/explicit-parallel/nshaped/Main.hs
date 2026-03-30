{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Data.Proxy
import GHC.TypeLits

--    f1  g1
--    |  /|
--    | / |
--    |/  |
--    f2  g2

data A deriving (Show, Read)
data B deriving (Show, Read)
data C deriving (Show, Read)
data D deriving (Show, Read)

f1 :: IO A
f1 = undefined

g1 :: IO B
g1 = undefined

f2 :: A -> B -> IO C
f2 = undefined

g2 ::B -> IO D
g2 = undefined

$(mkLoc "alice")
$(mkLoc "bob")

foo :: Choreo IO (C @ "alice", D @ "bob")
foo = do
  x <- alice `locally` (\_ -> newEmptyTMVarIO)
  (,) <$> c1 x <*> c2 x
  where
    c1 :: TMVar B @ "alice" -> Choreo IO (C @ "alice")
    c1 x = do
      a <- alice `locally` (\_ -> f1)
      alice `locally` (\un -> do
        b <- atomically $ takeTMVar (un x)
        f2 (un a) b)

    c2 :: TMVar B @ "alice" -> Choreo IO (D @ "bob")
    c2 x = do
      b <- bob `locally` (\_ -> g1)
      b' <- b ~> alice
      alice `locally` (\un -> atomically $ putTMVar (un x) (un b'))
      bob `locally` (\un -> do
        g2 (un b))

main :: IO ()
main = putStrLn "hello, world!"
