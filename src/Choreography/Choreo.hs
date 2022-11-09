{-# LANGUAGE OverloadedStrings #-}

module Choreography.Choreo where

import Choreography.Control
import Choreography.Location
import Control.Concurrent.Async

data Choreo a where
    Pure :: a -> Choreo a
    Comm :: (Show x, Read x) => x -> Location -> Location -> (x -> Choreo a) -> Choreo a

instance Functor Choreo where
    fmap f (Pure a)       = Pure (f a)
    fmap f (Comm x s r k) = Comm x s r (fmap f . k)

instance Applicative Choreo where
    pure = Pure

    f <*> x = f >>= \f -> x >>= \x -> return (f x)

instance Monad Choreo where
    (Pure a)       >>= f = f a
    (Comm x s r k) >>= f = k x >>= f

runChoreo :: Choreo a -> a
runChoreo (Pure a)     = a
runChoreo (Comm x s r k) = runChoreo (k x)

comm :: (Show x, Read x) => x -> Location -> Location -> Choreo x
comm x s r = Comm x s r Pure

price :: String -> Int
price "Types and Programming Languages" = 80
price "Homotopy Type Theory"            = 120

budget :: Int
budget = 100

type Date = Int

deliveryDate :: String -> Date
deliveryDate "Types and Programming Languages" = 1219
deliveryDate "Homotopy Type Theory"            = 1010

foo :: Choreo (Maybe Date)
foo = do
    title <- comm "Types and Programming Languages" "Buyer" "Seller"
    price <- comm (price title) "Seller" "Buyer"
    if price < budget
    then do
        date <- comm (deliveryDate title) "Seller" "Buyer"
        return (Just date)
    else do
        return Nothing

epp :: Choreo a -> Location -> Control a
epp (Comm x s r k) l
    | l == s    = send x r >> epp (k x) l
    | l == r    = recv s >>= \r -> epp (k r) l
    | otherwise = epp (k x) l
epp (Pure a) l = return a


example :: IO ()
example = do
    let buyer  = epp foo "Buyer"
        seller = epp foo "Seller"
    bufs <- newEmptyMsgBufs ["Buyer", "Seller"]
    async1 <- runControl bufs "Buyer" buyer
    async2 <- runControl bufs "Seller" seller
    result1 <- wait async1
    result2 <- wait async2
    print result1