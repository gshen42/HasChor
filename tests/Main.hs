{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import System.Environment

data Alice

data Bob

data Carol

data Leader

consensus :: ChoreoIO [Alice, Bob, Carol, Leader] (Bool @ Leader)
consensus = do
  -- the leader makes a proposal and sends it to all the participants
  proposalL <- locally @Leader makeProposal

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

  -- the leader checks if a quorum of participants accept the proposal. If not,
  -- recurse to retry a new proposal.
  quorumOrNot <- locally @Leader (checkResponses responseL1 responseL2 responseL3)

  cond quorumOrNot $ \case
    True -> return quorumOrNot
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

checkResponses ::
  Async Bool @ l ->
  Async Bool @ l ->
  Async Bool @ l ->
  Unwrap l ->
  IO Bool
checkResponses x y z un = do
  xx <- liftIO $ async $ checkTwo (un x) (un y)
  yy <- liftIO $ async $ checkTwo (un y) (un z)
  zz <- liftIO $ async $ checkTwo (un x) (un z)

  (_, a) <- liftIO $ waitAny [xx, yy, zz]

  return a

checkTwo :: Async Bool -> Async Bool -> IO Bool
checkTwo a1 a2 = do
  x <- wait a1
  y <- wait a2
  return (x && y)

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
