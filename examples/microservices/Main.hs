{-# LANGUAGE DataKinds #-}

module Main where

import Data.Binary
import Choreography
import GHC.TypeLits.Singletons

keyService :: SSymbol "keyService"
keyService = SSymbol @"keyService"

contentService :: SSymbol "contentService"
contentService = SSymbol @"contentService"

client :: SSymbol "client"
client = SSymbol @"client"

server :: SSymbol "server"
server = SSymbol @"server"

microServices :: Choreo IO ()
microServices = runService contentService getText display `par` runService keyService getKey decrypt
  where
    runService :: (KnownSymbol l, Binary a) => SSymbol l -> IO a -> (a -> IO b)-> Choreo IO ()
    runService service action handle = do
      x <- (service, \_ ->  action) ~~> server
      y <- x ~> client
      client `locally` (\un -> handle (un y))
      return ()

    getText :: IO String
    getText = getLine

    getKey :: IO String
    getKey = getLine

    display :: String -> IO ()
    display t = putStrLn ("Text: " ++ t)

    decrypt :: String -> IO ()
    decrypt k = putStrLn ("Key: " ++ k)

main :: IO ()
main = putStrLn "hello, world"
