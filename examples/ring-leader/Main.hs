{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment

-- an edge of the ring is represented as a tuple of two locaitons l and l' where
-- l is on the left of l'
data Edge = forall l l'.
  (KnownSymbol l, KnownSymbol l') => Edge (Proxy l) (Proxy l')

-- a ring is a sequence of edges
type Ring = [Edge]

type Label = Int

ringLeader :: Ring -> Choreo (StateT Label IO) ()
ringLeader ring = loop ring
  where
    loop :: Ring -> Choreo (StateT Label IO) ()
    loop []     = loop ring
    loop (x:xs) = do
      finished <- talkToRight x
      if finished
      then return ()
      else loop xs

    talkToRight :: Edge -> Choreo (StateT Label IO) Bool
    talkToRight (Edge left right) = do
      labelLeft  <- (left, \_ -> get) ~~> right
      labelRight <- right `locally` \_ -> get

      finished <- right `locally` \un ->
        return $ un labelLeft == un labelRight

      cond (right, finished) \case
        True  -> do
          right `locally` \_ -> lift $ putStrLn "I'm the leader"
          return True
        False -> do
          right `locally` \un -> put (max (un labelLeft) (un labelRight))
          return False

$(mkLoc "nodeA")
$(mkLoc "nodeB")
$(mkLoc "nodeC")
$(mkLoc "nodeD")

ring = [ Edge nodeA nodeB
       , Edge nodeB nodeC
       , Edge nodeC nodeD
       , Edge nodeD nodeA
       ]

main :: IO ()
main = do
  [loc] <- getArgs
  putStrLn "Please input a label:"
  label <- read <$> getLine
  runStateT (runChoreography config (ringLeader ring) loc) label
  return ()
  where
    config = mkHttpConfig [ ("nodeA", ("localhost", 4242))
                          , ("nodeB", ("localhost", 4343))
                          , ("nodeC", ("localhost", 4444))
                          , ("nodeD", ("localhost", 4545))
                          ]
