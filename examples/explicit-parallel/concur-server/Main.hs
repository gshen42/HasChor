{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits
import System.Environment

$(mkLoc "client1")
$(mkLoc "client2")
$(mkLoc "server")

concurServer :: Choreo IO ()
concurServer = session client1 `par` session client2
  where
    session :: (KnownSymbol l) => Proxy l -> Choreo IO ()
    session client = do
      x <- (client, \_ -> getLine) ~~> server
      y <- (server, \un -> return (un x ++ " from the server")) ~~> client
      _ <- client `locally` (\un -> putStrLn (un y))
      return ()

main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg concurServer loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("client1", ("localhost", 4242))
      , ("client2", ("localhost", 4343))
      , ("server", ("localhost", 4444))
      ]
