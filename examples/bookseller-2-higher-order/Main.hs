{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

$(mkLoc "buyer")
$(mkLoc "seller")
$(mkLoc "buyer2")

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes a choreography `mkDecision` that implements the decision making process.
bookseller :: (Int @ "buyer" -> Choreo IO (Bool @ "buyer")) -> Choreo IO (Maybe Day @ "buyer")
bookseller mkDecision = do
  -- the buyer reads the title of the book and sends it to the seller
  title <- (buyer, \_ -> do
               putStrLn "Enter the title of the book to buy"
               getLine
           )
           ~~> seller

  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un title)) ~~> buyer

  -- the buyer makes a decision using the `mkDecision` choreography
  decision <- mkDecision price

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  cond (buyer, decision) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un title)) ~~> buyer

      buyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un deliveryDate)
        return $ Just (un deliveryDate)

    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

-- | `mkDecision1` checks if buyer's budget is greater than the price of the book
mkDecision1 :: Int @ "buyer" -> Choreo IO (Bool @ "buyer")
mkDecision1 price = do
  buyer `locally` \un -> return $ un price < budget

-- | `mkDecision2` asks buyer2 how much they're willing to contribute and checks
-- if the buyer's budget is greater than the price of the book minus buyer2's contribution
mkDecision2 :: Int @ "buyer" -> Choreo IO (Bool @ "buyer")
mkDecision2 price = do
  contrib <- (buyer2, \_ -> do
                 putStrLn "How much you're willing to contribute?"
                 read <$> getLine
             )
             ~~> buyer
  buyer `locally` \un -> return $ un price - un contrib <= budget

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer"  -> runChoreography cfg choreo "buyer"
    "seller" -> runChoreography cfg choreo "seller"
    "buyer2" -> runChoreography cfg choreo "buyer2"
  return ()
  where
    choreo = bookseller mkDecision2

    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       , ("buyer2", ("localhost", 4444))
                       ]
