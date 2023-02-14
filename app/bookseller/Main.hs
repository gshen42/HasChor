{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.Proxy
import Data.Time
import System.Environment

buyer :: Proxy "buyer"
buyer = Proxy

buyer2 :: Proxy "buyer2"
buyer2 = Proxy

seller :: Proxy "seller"
seller = Proxy

budget :: Int
budget = 100

price :: String -> Int
price "Types and Programming Languages" = 80
price "Homotopy Type Theory"            = 120

deliveryDate :: String -> Day
deliveryDate "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDate "Homotopy Type Theory"            = fromGregorian 2023 01 01

mkDecision1 :: Int @ "seller" -> Choreo IO (Bool @ "buyer")
mkDecision1 price = do
  p <- (seller, price) ~> buyer
  decision <- buyer `locallyDo` \unwrap ->
    return $ budget >= unwrap p
  return decision

mkDecision2 :: Int @ "seller" -> Choreo IO (Bool @ "buyer")
mkDecision2 price = do
  p1 <- (seller, price) ~> buyer
  p2 <- (seller, price) ~> buyer2
  p2' <- buyer2 `locallyDo` (\unwrap -> return $ (unwrap p2) `div` 2)
  contrib <- (buyer2, p2') ~> buyer
  buyer `locallyDo` \unwrap ->
    return $ unwrap p1 - unwrap contrib < budget

bookseller :: (Int @ "seller" -> Choreo IO (Bool @ "buyer")) -> Choreo IO (Maybe Day @ "buyer")
bookseller mkDecision = do
  title <- buyer `locallyDo` (\_ -> getLine)
  t <- (buyer, title) ~> seller
  price <- seller `locallyDo` \unwrap -> do
    return $ price (unwrap t)
  return price
  -- p <- (seller, price) ~> buyer
  -- decision <- buyer `locallyDo` \unwrap ->
  --   return $ budget >= unwrap p
  decision <- mkDecision price 
  cond buyer decision \case
    True -> do
      date <- seller `locallyDo` \unwrap -> return $ deliveryDate (unwrap t)
      d <- (seller, date) ~> buyer
      buyer `locallyDo` \unwrap -> do
        putStrLn $ show (unwrap d)
        return (Just (unwrap d))
    False ->
      buyer `locallyDo` \unwrap -> return Nothing

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
         "buyer" -> runNetwork config "buyer" buyerP
         "seller" -> runNetwork config "seller" sellerP
  return ()
  where
    buyerP = epp (bookseller mkDecision1) "buyer"

    sellerP = epp (bookseller mkDecision1) "seller"

    config = mkConfig [ ("buyer",  ("localhost", 4242))
                      , ("seller", ("localhost", 4343))
                      ]

