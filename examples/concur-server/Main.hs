{-# LANGUAGE DataKinds #-}

module Main where

import Choreography
import GHC.TypeLits.Singletons

client1 :: SSymbol "client1"
client1 = SSymbol @"client1"

client2 :: SSymbol "client2"
client2 = SSymbol @"client2"

server :: SSymbol "server"
server = SSymbol @"server"

concurServer :: Choreo IO ()
concurServer = session client1 `par` session client2
  where
    session :: (KnownSymbol l) => SSymbol l -> Choreo IO ()
    session client = do
      x <- (client, \_ -> getLine) ~~> server
      y <- (server, \un -> return (un x ++ " from the server")) ~~> client
      _ <- client `locally` (\un -> putStrLn (un y))
      return ()

main :: IO ()
main = putStrLn "hello, world"
