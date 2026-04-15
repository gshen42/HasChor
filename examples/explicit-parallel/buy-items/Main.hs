{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.TypeLits
import System.Environment
import Control.Concurrent (MVar, newMVar, tryTakeMVar)
import Control.Monad.Trans.Maybe

$(mkLoc "seller")
$(mkLoc "buyer1")
$(mkLoc "buyer2")

-- Ozone: Figure 5
chooseBuyItems :: Choreo IO ()
chooseBuyItems = do
  items <- seller `locally` (\_ -> takeInventory)
  chooseBuyItem items seller buyer1 `par` chooseBuyItem items seller buyer2
  where
    chooseBuyItem :: (KnownSymbol s, KnownSymbol b) =>
      (HashMap Int (MVar ()) @ s) -> Proxy s -> Proxy b -> Choreo IO ()
    chooseBuyItem items s b = do
      itemId <- b `locally` (\_ -> chooseItem)
      buyItem items s b itemId

    buyItem :: (KnownSymbol s, KnownSymbol b) =>
      (HashMap Int (MVar ()) @ s) -> Proxy s -> Proxy b -> (Int @ b) -> Choreo IO ()
    buyItem items s b itemId = do
      sItemId <- itemId ~> s
      sItem <- s `locally` (\un -> sell (un items) (un sItemId))
      bItem <- sItem ~> b
      b `locally` (\un -> print (un bItem))
      return ()

    chooseItem :: IO Int
    chooseItem = putStrLn "Enter an item id: " >> read <$> getLine

    sell :: HashMap Int (MVar ()) -> Int -> IO (Maybe ())
    sell items itemId = runMaybeT $ do
      itemVar <- hoistMaybe $ HashMap.lookup itemId items
      MaybeT $ tryTakeMVar itemVar

    takeInventory :: IO (HashMap Int (MVar ()))
    takeInventory = HashMap.fromList <$> mapM (\i -> (i,) <$> newMVar ()) [0..10]


main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg chooseBuyItems loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("seller", ("localhost", 4242))
      , ("buyer1", ("localhost", 4343))
      , ("buyer2", ("localhost", 4444))
      ]
