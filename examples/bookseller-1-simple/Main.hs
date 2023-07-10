{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo IO (Maybe Day @ "buyer")
bookseller = do
  -- the buyer node prompts the user to enter the title of the book to buy
  title <-
    buyer `locally` \_ -> do
      putStrLn "Enter the title of the book to buy"
      getLine
  -- the buyer sends the title to the seller
  title' <- (buyer, title) ~> seller

  -- the seller checks the price of the book
  price <- seller `locally` \un -> return $ priceOf (un title')
  -- the seller sends back the price of the book to the buyer
  price' <- (seller, price) ~> buyer

  -- the buyer decides whether to buy the book or not
  decision <- buyer `locally` \un -> return $ (un price') < budget

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  cond (buyer, decision) \case
    True  -> do
      deliveryDate  <- seller `locally` \un -> return $ deliveryDateOf (un title')
      deliveryDate' <- (seller, deliveryDate) ~> buyer

      buyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un deliveryDate')
        return $ Just (un deliveryDate')

    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

-- `bookseller'` is a simplified version of `bookseller` that utilizes `~~>`
bookseller' :: Choreo IO (Maybe Day @ "buyer")
bookseller' = do
  title <- (buyer, \_ -> do
               putStrLn "Enter the title of the book to buy"
               getLine
           )
           ~~> seller

  price <- (seller, \un -> return $ priceOf (un title)) ~~> buyer

  cond' (buyer, \un -> return $ (un price) < budget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un title)) ~~> buyer

      buyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un deliveryDate)
        return $ Just (un deliveryDate)

    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

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
    "buyer"  -> runChoreography cfg bookseller' "buyer"
    "seller" -> runChoreography cfg bookseller' "seller"
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
