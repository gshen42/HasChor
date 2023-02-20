{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Time
import System.Environment
import System.Exit (exitSuccess)

client :: Proxy "client"
client = Proxy

server :: Proxy "server"
server = Proxy

type S = Map String String

data Req = Put String String | Get String deriving (Show, Read)

parseReq :: String -> Maybe Req
parseReq s =
  let l = words s
   in case l of
        ["GET", k] -> Just (Get k)
        ["PUT", k, v] -> Just (Put k v)
        _ -> Nothing

kvs :: S @ "server" -> Choreo IO (S @ "server")
kvs s = do
  cmdC <-
    client `locally` \unwrap -> do
      let loop = do
            putStrLn "Command?"
            line <- getLine
            let x = parseReq line
             in case x of
                  Just t -> return t
                  Nothing -> do
                    putStrLn "Invalid command"
                    loop
      loop
  cmdS <- (client, cmdC) ~> server
  x <-
    server `locally` \unwrap -> case unwrap cmdS of
      Put k v -> do
        return $ (Map.insert k v (unwrap s), Just "OK")
      Get k -> do
        let e = Map.lookup k (unwrap s)
         in do
              return $ (unwrap s, e)
  s' <- server `locally` \unwrap -> do return $ fst (unwrap x)
  t <- server `locally` \unwrap -> do return $ snd (unwrap x)
  t' <- (server, t) ~> client
  client `locally` \unwrap -> do
    putStrLn $ show (unwrap t')
  return s'

kvsServer :: Choreo IO ()
kvsServer = do
  initS <- server `locally` \unwrap -> return (Map.empty :: S)
  let loop (s :: S @ "server") = do
        s' <- kvs s
        loop s'
   in loop initS
  return ()

kvsClient :: Choreo IO ()
kvsClient = do
  initS <- server `locally` \unwrap -> return (Map.empty :: S)
  kvs initS
  -- TODO(shun): program should exit without this
  client `locally` \unwrap -> do
    threadDelay 10000 -- without this, the server crashes
    exitSuccess
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "client" -> runChoreography config kvsClient "client"
    "server" -> runChoreography config kvsServer "server"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 5000)),
          ("server", ("localhost", 5001))
        ]
