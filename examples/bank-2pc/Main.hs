{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import System.Environment
import Text.Read (readMaybe)
import Data.Proxy

$(mkLoc "client")
$(mkLoc "coordinator")
$(mkLoc "alice")
$(mkLoc "bob")

type State = (Int @ "alice", Int @ "bob")

type Action = (String, Int)

type Transaction = [Action]

-- | `validate` checks if a transaction can be executed while keeping balance >= 0
-- returns if the transaction satisfies the property and the balance after the transaction
validate :: String -> Int -> Transaction -> (Bool, Int)
validate name balance tx = foldl (\(valid, i) (_, amount) -> (let next = i + amount in (valid && next >= 0, next))) (True, balance) actions
  where
    actions = filter (\(n, _) -> n == name) tx

-- | `parse` converts the user input into a transaction
parse :: String -> Transaction
parse s = tx
  where
    t = splitOn ";" s
    f :: String -> Maybe Action
    f l = do
      [target, amountStr] <- return $ words l
      amount <- readMaybe amountStr :: Maybe Int
      target' <- if target == "alice" || target == "bob" then Just target else Nothing
      return (target', amount)
    tx = mapMaybe f t

-- | `handleTransaction` is a choreography that handles a transaction.
-- Given the current state and a transaction, it will first ask alice and bob to vote,
-- then it will decide whether to commit the transaction or not.
-- If the transaction is committed, it will update the state.
-- Otherwise, it will keep the state unchanged.
handleTransaction :: State -> Transaction @ "coordinator" -> Choreo IO (Bool @ "coordinator", State)
handleTransaction (aliceBalance, bobBalance) tx = do
  -- Voting Phase
  txa <- (coordinator, tx) ~> alice
  voteAlice <- (alice, \unwrap -> do { return $ fst $ validate "alice" (unwrap aliceBalance) (unwrap txa) }) ~~> coordinator
  txb <- (coordinator, tx) ~> bob
  voteBob <- (bob, \unwrap -> do { return $ fst $ validate "bob" (unwrap bobBalance) (unwrap txb) }) ~~> coordinator

  -- Check if the transaction can be committed
  canCommit <- coordinator `locally` \unwrap -> do return $ unwrap voteAlice && unwrap voteBob

  -- Commit Phase
  cond (coordinator, canCommit) \case
    True -> do
      aliceBalance' <- alice `locally` \unwrap -> do return $ snd $ validate "alice" (unwrap aliceBalance) (unwrap txa)
      bobBalance' <- bob `locally` \unwrap -> do return $ snd $ validate "bob" (unwrap bobBalance) (unwrap txb)
      return (canCommit, (aliceBalance', bobBalance'))
    False -> do
      return (canCommit, (aliceBalance, bobBalance))

-- | `bank` loops forever and handles transactions.
bank :: State -> Choreo IO ()
bank state = do
  client `locally` \_ -> do
    putStrLn "Command? (alice|bob {amount};)+"
  tx <- (client, \_ -> do { parse <$> getLine }) ~~> coordinator
  (committed, state') <- handleTransaction state tx
  committed' <- (coordinator, committed) ~> client
  client `locally` \unwrap -> do
    putStrLn if unwrap committed' then "Committed" else "Not committed"
  alice `locally` \unwrap -> do putStrLn ("Alice's balance: " ++ show (unwrap (fst state')))
  bob `locally` \unwrap -> do putStrLn ("Bob's balance: " ++ show (unwrap (snd state')))
  bank state' -- repeat
  return ()

-- | `startBank` is a choreography that initializes the states and starts the bank application.
startBank :: Choreo IO ()
startBank = do
  aliceBalance <- alice `locally` \_ -> do return 0
  bobBalance <- bob `locally` \_ -> do return 0
  bank (aliceBalance, bobBalance)

main :: IO ()
main = do
  runChoreo startBank
