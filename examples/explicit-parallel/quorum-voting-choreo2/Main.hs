{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography2
import Control.Concurrent.STM
import Control.Monad
import Data.Proxy
import GHC.TypeLits
import System.Environment

$(mkLoc "voter1")
$(mkLoc "voter2")
$(mkLoc "voter3")
$(mkLoc "candidate")

quorumVoting :: Choreo IO l' ()
quorumVoting = do
  yesVotes <- candidate `locally` (\_ -> newTVarIO 0)
  noVotes <- candidate `locally` (\_ -> newTVarIO 0)
  castVote voter1 yesVotes noVotes `par`
    castVote voter2 yesVotes noVotes `par`
      castVote voter3 yesVotes noVotes `par`
        countYesVotes yesVotes `par`
          countNoVotes noVotes
  where
    castVote :: (KnownSymbol l) =>
      SSymbol l ->
      At (TVar Int) "candidate" l' ->
      At (TVar Int) "candidate" l' ->
      Choreo IO l' (At () "candidate" l')
    castVote voter yesVotes noVotes = do
      vote <- (voter, \_ -> getVote) ~~> candidate
      candidate `locally` \un -> do
        if un vote then
          atomically $ modifyTVar (un yesVotes) (+1)
        else
          atomically $ modifyTVar (un noVotes) (+1)

    getVote :: IO Bool
    getVote = read <$> getLine

    countYesVotes :: At (TVar Int) "candidate" l' -> Choreo IO l' ()
    countYesVotes yesVotes =
      void $ candidate `locally` \un -> do
        atomically (do x <- readTVar (un yesVotes); check (x >= 2))
        putStrLn "I'm selected"

    countNoVotes :: At (TVar Int) "candidate" l' -> Choreo IO l' ()
    countNoVotes noVotes =
      void $ candidate `locally` \un -> do
        atomically (do x <- readTVar (un noVotes); check (x >= 2))
        putStrLn "I'm not selected"

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "voter1" -> runChoreography cfg quorumVoting voter1
    "voter2" -> runChoreography cfg quorumVoting voter2
    "voter3" -> runChoreography cfg quorumVoting voter3
    "candidate" -> runChoreography cfg quorumVoting candidate
  where
    cfg = mkHttpConfig
      [ ("voter1", ("localhost", 4242))
      , ("voter2", ("localhost", 4343))
      , ("voter3", ("localhost", 4444))
      , ("candidate", ("localhost", 4545))
      ]
