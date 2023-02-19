{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

buyer2 :: Proxy "buyer2"
buyer2 = Proxy

withinBudget :: Monad m => Int -> m Bool
withinBudget x = return (x <= 100)

lookupPrice :: Monad m => String -> m Int
lookupPrice "Types and Programming Languages" = return 80
lookupPrice "Homotopy Type Theory"            = return 120

deliveryDate :: Monad m => String -> m Day
deliveryDate "Types and Programming Languages" = return (fromGregorian 2022 12 19)
deliveryDate "Homotopy Type Theory"            = return (fromGregorian 2023 01 01)

bookseller :: (Int @ "buyer" -> Choreo IO (Bool @ "buyer")) -> Choreo IO (Maybe Day @ "buyer")
bookseller mkDecision = do
  title <- (buyer, \_ -> getLine) ~~> seller
  price <- (seller, \un -> lookupPrice (un title)) ~~> buyer
  decision <- mkDecision price
  cond (buyer, decision) \case
    True -> do
      date <- (seller, \un -> deliveryDate (un title)) ~~> buyer
      buyer `locally` \un -> do
        print (un date)
        return (Just (un date))
    False ->
      buyer `locally` \_ -> return Nothing

mkDecision1 :: Int @ "buyer" -> Choreo IO (Bool @ "buyer")
mkDecision1 price = do
  buyer `locally` \un -> withinBudget (un price)

mkDecision2 :: Int @ "buyer" -> Choreo IO (Bool @ "buyer")
mkDecision2 price = do
  buyer2 `locally` \_ -> do
    putStrLn "How much you're willing to contribute?"
  contrib <- (buyer2, \_ -> read <$> getLine) ~~> buyer
  buyer `locally` \un -> withinBudget (un price - un contrib)

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer"   -> runChoreography cfg choreo "buyer"
    "seller"  -> runChoreography cfg choreo "seller"
    "buyer2"  -> runChoreography cfg choreo "buyer2"
  return ()
  where
    choreo = bookseller mkDecision1

    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
--                       , ("buyer2", ("localhost", 4444))
                       ]
