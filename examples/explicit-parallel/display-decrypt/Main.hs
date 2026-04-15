{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits
import System.Environment

$(mkLoc "contentService")
$(mkLoc "keyService")
$(mkLoc "client")
$(mkLoc "server")

-- Ozone: Figure 2-4
decryptDisplay :: Choreo IO ()
decryptDisplay = contentSession `par` keySession
    where
      contentSession :: Choreo IO ()
      contentSession = do
        stxt <- (contentService, \_ -> getText) ~~> server
        ctxt <- stxt ~> client
        client `locally` (\un -> display (un ctxt))
        return ()

      keySession :: Choreo IO ()
      keySession = do
        skey <- (keyService, \_ -> getKey) ~~> server
        ckey <- skey ~> client
        client `locally` (\un -> decrypt (un ckey))
        return ()

      getText :: IO String
      getText = do
        putStrLn "Enter some text:"
        getLine

      getKey :: IO String
      getKey = do
        putStrLn "Enter the decryption key:"
        getLine

      display :: String -> IO ()
      display s = putStrLn $ "Display: " ++ s

      decrypt :: String -> IO ()
      decrypt s = putStrLn $ "Decrypt using key: " ++ s

main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg decryptDisplay loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("contentService", ("localhost", 4242))
      , ("keyService", ("localhost", 4343))
      , ("server", ("localhost", 4444))
      , ("client", ("localhost", 4445))
      ]
