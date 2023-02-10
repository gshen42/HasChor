{-# LANGUAGE OverloadedStrings #-}

module Main where

import Choreography.Location
import Choreography.Network
import Choreography.Network.Http
import Data.Time
import Control.Concurrent

title :: String
title  = "Types and Programming Languages"

budget :: Int
budget = 100

price :: String -> Int
price "Types and Programming Languages" = 80
price "Homotopy Type Theory"            = 120

deliveryDate :: String -> Day
deliveryDate "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDate "Homotopy Type Theory"            = fromGregorian 2023 01 01


main :: IO ()
main = do
  let cfg = mkConfig [ ("buyer",  ("localhost", 4200))
                     , ("seller", ("localhost", 4201))
                     ]
  forkIO $ runNetwork cfg "seller" seller
  d <- runNetwork cfg "buyer" buyer
  print d
  where
    buyer :: Network IO (Maybe Day)
    buyer = do
      send title "seller"
      price <- recv "seller"
      if price <= budget
      then do
        send True "seller"
        date <- recv "seller"
        return (Just date)
      else do
        send False "seller"
        return Nothing

    seller :: Network IO ()
    seller = do
      title <- recv "buyer"
      send (price title) "buyer"
      decision <- recv "buyer"
      if decision
      then do
        send (deliveryDate title) "buyer"
        return ()
      else do
        return ()
