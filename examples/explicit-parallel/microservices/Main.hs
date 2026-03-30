{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits

$(mkLoc "keyService")
$(mkLoc "contentService")
$(mkLoc "client")
$(mkLoc "server")

microServices :: Choreo IO ()
microServices = runService contentService getText display `par` runService keyService getKey decrypt
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
main = putStrLn "hello, world"
