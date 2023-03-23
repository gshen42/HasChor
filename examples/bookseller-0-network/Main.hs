module Main where

import Choreography.Network
import Choreography.Network.Http
import Data.Time
import System.Environment

buyer :: Network IO ()
buyer = do
  run $ putStrLn "Enter the title of the book to buy:"
  title <- run getLine
  send title "seller"
  price <- recv "seller"
  if price < budget
  then do
    send True "seller"
    (deliveryDate :: Day) <- recv "seller"
    run $ putStrLn ("The book will be delivered on " ++ (show deliveryDate))
  else do
    send False "seller"
    run $ putStrLn "The book's price is out of the budget"

seller :: Network IO ()
seller = do
  title <- recv "buyer"
  send (priceOf title) "buyer"
  decision <- recv "buyer"
  if decision
  then do
    send (deliveryDateOf title) "buyer"
  else do
    return ()

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2023 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 09 18

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer" -> runNetwork cfg "buyer" buyer
    "seller" -> runNetwork cfg "seller" seller
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
