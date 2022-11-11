{-# LANGUAGE OverloadedStrings #-}

module Main where

import Choreography.Choreo
import Choreography.Control
import Choreography.Location
import Data.Time

controlExample :: IO [(Location, Maybe Day)]
controlExample = runControl [("buyer", buyer), ("seller", seller)]
    where
        buyer :: Control (Maybe Day)
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

        seller :: Control (Maybe Day)
        seller = do
            title <- recv "buyer"
            send (price title) "buyer"
            decision <- recv "buyer"
            if decision
            then do
                send (deliveryDate title) "buyer"
                return Nothing
            else do
                return Nothing

choreoExample :: IO [(Location, Maybe Day)]
choreoExample = do
    let buyer  = epp choreo "buyer"
        seller = epp choreo "seller"
    runControl [("buyer", buyer), ("seller", seller)]
    where
        choreo :: Choreo (Maybe Day)
        choreo = do
            t <- comm title "buyer" "seller"
            p <- comm (price t) "seller" "buyer"
            if p < budget
            then do
                d <- comm (deliveryDate t) "seller" "buyer"
                return (Just d)
            else do
                return Nothing

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
    result <- choreoExample
    print result
