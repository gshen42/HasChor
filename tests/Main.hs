{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography
import Choreography.Macro
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import System.Environment

data Alice

data Bob

data Carol

data Leader

consensus :: ChoreoIO [Alice, Bob, Carol, Leader] (Bool @ Leader)
consensus = do
  -- the leader makes a proposal and sends it to all the participants
  proposalL <- locally @Leader makeProposal

  -- forEach @[Alice, Bob, Carol] $ \(_ :: Proxy l) -> do
  --   comm @Leader @l proposalL
  --   return ()

  proposalA <- comm @Leader @Alice proposalL
  proposalB <- comm @Leader @Bob proposalL
  proposalC <- comm @Leader @Carol proposalL

  -- paticipants decide whether to accept the proposal and inform the leader
  responseA <- locally @Alice (evalProposal proposalA)
  responseL1 <- comm @Alice @Leader responseA

  responseB <- locally @Bob (evalProposal proposalB)
  responseL2 <- comm @Bob @Leader responseB

  responseC <- locally @Carol (evalProposal proposalC)
  responseL3 <- comm @Carol @Leader responseC

  -- the leader checks if a quorum of participants accept or refute the proposal
  acceptOrRefute <- locally @Leader (checkQuorum responseL1 responseL2 responseL3)

  -- recurse if no quorum of accept
  cond acceptOrRefute $ \case
    True -> return acceptOrRefute
    False -> consensus

makeProposal :: Unwrap l -> IO String
makeProposal un = do
  putStrLn "Make a new proposal:"
  getLine

evalProposal :: Async String @ l -> Unwrap l -> IO Bool
evalProposal pAtl un = do
  p <- wait (un pAtl)
  putStrLn $ "The leader's latest proposal is: " ++ p
  putStrLn "Do you accept the proposal? (please type in True or False)"
  read <$> getLine

checkQuorum ::
  Async Bool @ l ->
  Async Bool @ l ->
  Async Bool @ l ->
  Unwrap l ->
  IO Bool
checkQuorum x y z un = do
  counterTrue <- newTVarIO 0
  counterFalse <- newTVarIO 0

  async $ do
    xx <- wait (un x)
    atomically $ modifyTVar (if xx then counterTrue else counterFalse) (+ 1)
  async $ do
    yy <- wait (un y)
    atomically $ modifyTVar (if yy then counterTrue else counterFalse) (+ 1)
  async $ do
    zz <- wait (un z)
    atomically $ modifyTVar (if zz then counterTrue else counterFalse) (+ 1)

  atomically $ do
    x <- readTVar counterTrue
    y <- readTVar counterFalse

    if x >= 2
      then return True
      else
        if y >= 2
          then return False
          else retry

main :: IO ()
main = do
  [loc] <- getArgs
  void $ case loc of
    "Leader" -> runChoreo @Leader cfg consensus
    "Alice" -> runChoreo @Alice cfg consensus
    "Bob" -> runChoreo @Bob cfg consensus
    "Carol" -> runChoreo @Carol cfg consensus
  where
    cfg =
      mkHttpConfig
        [ ("Leader", ("localhost", 8000)),
          ("Alice", ("localhost", 8001)),
          ("Bob", ("localhost", 8002)),
          ("Carol", ("localhost", 8003))
        ]
