{-# LANGUAGE DataKinds #-}

module Main where

import Choreography
import GHC.TypeLits.Singletons
import Control.Concurrent.STM
import Control.Monad

voter1 :: SSymbol "voter1"
voter1 = SSymbol @"voter1"

voter2 :: SSymbol "voter2"
voter2 = SSymbol @"voter2"

voter3 :: SSymbol "voter3"
voter3 = SSymbol @"voter3"

candidate :: SSymbol "candidate"
candidate = SSymbol @"candidate"

quorumVoting :: Choreo IO ()
quorumVoting = do
  yesVotes <- candidate `locally` (\_ -> newTVarIO 0)
  noVotes <- candidate `locally` (\_ -> newTVarIO 0)
  castVote voter1 yesVotes noVotes `par`
    castVote voter2 yesVotes noVotes `par`
      castVote voter3 yesVotes noVotes
  void $ countYesVotes yesVotes `par` countNoVotes noVotes
  where
    castVote :: (KnownSymbol l) => SSymbol l -> TVar Int @ "candidate" -> TVar Int @ "candidate" ->
      Choreo IO (() @ "candidate")
    castVote voter yesVotes noVotes = do
      vote <- (voter, \_ -> getVote) ~~> candidate
      candidate `locally` \un -> do
        if un vote then
          atomically $ modifyTVar (un yesVotes) (+1)
        else
          atomically $ modifyTVar (un noVotes) (+1)

    getVote :: IO Bool
    getVote = read <$> getLine

    countYesVotes :: TVar Int @ "candidate" -> Choreo IO ()
    countYesVotes yesVotes =
      void $ candidate `locally` \un -> do
        atomically (do x <- readTVar (un yesVotes); check (x >= 2))
        putStrLn "I'm selected"

    countNoVotes :: TVar Int @ "candidate" -> Choreo IO ()
    countNoVotes noVotes =
      void $ candidate `locally` \un -> do
        atomically (do x <- readTVar (un noVotes); check (x >= 2))
        putStrLn "I'm not selected"

main :: IO ()
main = putStrLn "hello, world"
