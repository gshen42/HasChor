{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Control.Monad
import Data.Proxy
import System.Environment

-- Step 1: Defining locations
$(mkLoc "alice")

-- Step 2: Writing a choreography
choreography :: Choreo IO (() @ "alice")
choreography = do
  alice `locally` \_ -> putStrLn "Hello, world!"

-- Step 3: Projecting and running the chreography
main :: IO ()
main = do
  args <- getArgs
  case args of
    [loc] -> void $ runChoreography cfg choreography loc
    _     -> error "wrong usage: must provide exactly one location"
  where
    -- Step 4: Mapping locations to HTTP ports
    cfg = mkHttpConfig [ ("alice", ("localhost", 4242))
                       ]
