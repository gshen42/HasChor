{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Choreography
import Data.Proxy
import GHC.TypeLits
import System.Environment
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

$(mkLoc "w1")
$(mkLoc "w2")
$(mkLoc "m1")
$(mkLoc "m2")
$(mkLoc "gateway")
$(mkLoc "batcher")

type Img = Int
type ImgId = Int
type Result = Int

imageClassification :: Choreo IO ()
imageClassification = do
  -- Need App and ChoreoSig exported to use
  -- (w1Imgs, w2Imgs) <- (,) <$> makeImageMap <*> makeImageMap
  w1Imgs <- w1 `locally` (\_ -> makeImageMap)
  w2Imgs <- w2 `locally` (\_ -> makeImageMap)
  ctr <- gateway `locally` (\_ -> makeCounter)
  distributeImages gateway w1 w2 ctr `par`
    sendRequests gateway batcher
  where
    makeImageMap :: IO (TMVar (HashMap ImgId Img))
    makeImageMap = undefined

    makeCounter :: IO (TMVar Int)
    makeCounter = newTMVarIO 0

    images :: [Img @ gateway]
    images = undefined

    distributeImages :: (KnownSymbol g, KnownSymbol w1, KnownSymbol w2) =>
      Proxy g -> Proxy w1 -> Proxy w2 -> TMVar Int @ g ->
      Choreo IO ()
    distributeImages g w1 w2 ctr =
      mapM_
      (\img -> do
        c <- g `locally` (\un ->
          let ctr' = un ctr in
          atomically (do
            c <- takeTMVar ctr'
            let c' = (c + 1) `mod` 2
            putTMVar ctr' c'
            return c))
        cond c (\c ->
          case c of
            0 -> sendImage img w1
            1 -> sendImage img w2
            _ -> undefined
          )
        )
      images

    sendImage = _
    sendRequests = _
    sendBatches = _


main :: IO ()
main = do
  [loc] <- getArgs
  runChoreography cfg imageClassification loc
  return ()
  where
    cfg = mkHttpConfig
      [ ("w1", ("localhost", 4242))
      , ("w2", ("localhost", 4343))
      , ("m1", ("localhost", 4344))
      , ("m2", ("localhost", 4345))
      , ("gateway", ("localhost", 4346))
      , ("batcher", ("localhost", 4347))
      ]
