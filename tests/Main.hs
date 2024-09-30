{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import System.Environment

data Alice

data Bob

data Carol

data Leader

makeProposal :: LocatedIO l String
makeProposal = liftIO getLine

makeDecision :: Async String @ l -> LocatedIO l Bool
makeDecision p = do
  p <- unwrap p
  p <- liftIO $ wait p
  liftIO $ putStrLn ("The leader's latest proposal is:" ++ p)
  liftIO $ putStrLn "Do you accept the proposal?"
  read <$> liftIO getLine

checkDecision :: Async Bool -> Async Bool -> IO Bool
checkDecision a1 a2 = do
  x <- wait a1
  y <- wait a2
  return (x && x)

consensusSimple :: ChoreoIO [Alice, Bob, Carol, Leader] (Bool @ Leader)
consensusSimple = do
  -- the leader makes a proposal and sends it to all the participants
  proposal <- locally @Leader makeProposal
  proposalA <- comm @Leader @Alice proposal
  proposalB <- comm @Leader @Bob proposal
  proposalC <- comm @Leader @Carol proposal

  -- paticipants decide whether to accept the proposal and inform the leader
  decisionA <- locally @Alice (makeDecision proposalA)
  decisionA' <- comm @Alice @Leader decisionA

  decisionB <- locally @Bob (makeDecision proposalB)
  decisionB' <- comm @Bob @Leader decisionB

  decisionC <- locally @Carol (makeDecision proposalC)
  decisionC' <- comm @Carol @Leader decisionC

  -- the leader check if consensus (a quorum of paticipants accept the proposal)
  -- has reached. If not, recurse to make a new proposal
  reached <- locally @Leader $ do
    x <- unwrap decisionA'
    y <- unwrap decisionB'
    z <- unwrap decisionC'
    xx <- liftIO $ async $ checkDecision x y
    yy <- liftIO $ async $ checkDecision y z
    zz <- liftIO $ async $ checkDecision x z

    (_, a) <- liftIO $ waitAny [xx, yy, zz]
    return a

  cond reached $ \case
    True -> return reached
    False -> consensusSimple

main :: IO ()
main = do
  [loc] <- getArgs
  void $ case loc of
    "Leader" -> runChoreo @Leader cfg consensusSimple
    "Alice" -> runChoreo @Alice cfg consensusSimple
    "Bob" -> runChoreo @Bob cfg consensusSimple
    "Carol" -> runChoreo @Carol cfg consensusSimple
  where
    cfg =
      mkHttpConfig
        [ ("Leader", ("localhost", 8000)),
          ("Alice", ("localhost", 8001)),
          ("Bob", ("localhost", 8002)),
          ("Carol", ("localhost", 8003))
        ]
