{-# LANGUAGE DataKinds #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Control.Concurrent.Future
import Control.Monad.Freer
import Data.Proxy

data Alice

data Bob

data Carol

c :: ChoreoIO [Alice, Bob, Carol] (Future Int @ Carol)
c = do
  fut <- comm @Alice @Bob $ read <$> getLine
  -- locally @Carol $ wait (un fut)
  comm @Bob @Carol $ do
    un fut `waitThen` \x ->
      return (x + 3)

main :: IO ()
main = printChoreo c >> return ()
