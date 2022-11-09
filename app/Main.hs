{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Choreography.Control
import Data.Time ( fromGregorian, Day )
import Control.Concurrent.Async (wait)

buyerGen :: String -> Int -> Control (Maybe Day)
buyerGen title budget = do
    send title "seller"
    (price :: Int) <- recv "seller"
    if price <= budget
    then do
        send True "seller"
        delivery_date <- recv "seller"
        return (Just delivery_date)
    else do
        send False "seller"
        return Nothing

sellerGen :: (String -> Int) -> (String -> Day) -> Control ()
sellerGen price delivery_date = do
    title <- recv "buyer"
    send (price title) "buyer"
    decision <- recv "buyer"
    if decision
    then do
        send (delivery_date title) "buyer"
    else do
        return ()

bookseller :: IO ()
bookseller = do
    bufs <- newEmptyMsgBufs ["buyer", "seller"]
    async1 <- runControl bufs "buyer" buyer
    async2 <- runControl bufs "seller" seller
    result1 <- wait async1
    result2 <- wait async2
    print result1
    where
        buyer = buyerGen "Types and Programming Languages" 100
        seller = sellerGen (\case "Types and Programming Languages" -> 80;
                                  "Homotopy Type Theory" -> 120)
                           (\case "Types and Programming Languages" -> (fromGregorian 2022 12 19)
                                  "Homotopy Type theory" -> (fromGregorian 2023 1 1))

main :: IO ()
main = bookseller
