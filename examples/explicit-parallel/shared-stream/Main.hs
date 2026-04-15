{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits
import System.Environment
import Control.Monad (when)

$(mkLoc "p1")
$(mkLoc "p2")
$(mkLoc "c")

-- Ozone: Figure 5
sharedStreams :: Choreo IO ()
sharedStreams = streamIt p1 c `par` streamIt p2 c
  where
    streamIt :: (KnownSymbol p, KnownSymbol c) =>
      Proxy p -> Proxy c -> Choreo IO ()
    streamIt p c = do
      cx <- (p, \_ -> produce) ~~> c
      cz <- c `locally` (\un -> consume (un cx))
      isItemsLeft <- p `locally` (\_ -> itemsLeft)
      cond isItemsLeft (\b -> when b (streamIt p c))

    produce :: IO Int
    produce = putStrLn "Enter an integer: " >> read <$> getLine

    consume :: Int -> IO ()
    consume x = putStrLn $ "Got: " ++ show x

    itemsLeft :: IO Bool
    itemsLeft = putStrLn "Press enter to continue or type anything to stop." >> null <$> getLine


main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg sharedStreams loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("p1", ("localhost", 4242))
      , ("p2", ("localhost", 4343))
      , ("c", ("localhost", 4344))
      ]
