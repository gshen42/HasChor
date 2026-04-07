{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits
import System.Environment

$(mkLoc "keyService")
$(mkLoc "contentService")
$(mkLoc "client")
$(mkLoc "server")

microServices :: Choreo IO ()
microServices = do
  runService contentService getText display
  runService keyService getKey decrypt
  return ()
  where
    runService :: (KnownSymbol l, Show a, Read a) => Proxy l -> IO a -> (a -> IO b)-> Choreo IO ()
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
main = do
  [loc] <- getArgs
  runChoreography cfg microServices loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("keyService", ("localhost", 4242))
      , ("contentService", ("localhost", 4343))
      , ("client", ("localhost", 4444))
      , ("server", ("localhost", 4545))
      ]
