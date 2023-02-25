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
import Data.IORef
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.IORef (IORef (IORef))
import GHC.TypeLits (KnownSymbol)
import System.Environment

client :: Proxy "client"
client = Proxy

primary :: Proxy "primary"
primary = Proxy

backup :: Proxy "backup"
backup = Proxy

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

isPut :: Request -> Bool
isPut request = case request of
  Put _ _ -> True
  _ -> False

type Response = Maybe String

readRequest :: IO Request
readRequest = do
  putStrLn "Command?"
  line <- getLine
  case parseRequest line of
    Just t -> return t
    Nothing -> putStrLn "Invalid command" >> readRequest
  where
    parseRequest :: String -> Maybe Request
    parseRequest s =
      let l = words s
       in case l of
            ["GET", k] -> Just (Get k)
            ["PUT", k, v] -> Just (Put k v)
            _ -> Nothing

handleRequest :: Request -> IORef State -> IO Response
handleRequest request stateRef = case request of
  Put key value -> do
    modifyIORef stateRef (Map.insert key value)
    return (Just value)
  Get key -> do
    state <- readIORef stateRef
    return (Map.lookup key state)

kvs ::
  Request @ "client" ->
  (IORef State @ "primary", IORef State @ "backup") ->
  Choreo IO (Response @ "client")
kvs request (primaryStateRef, backupStateRef) = do
  request' <- (client, request) ~> primary

  -- if the request is mutating (PUT)
  m <- primary `locally` \unwrap -> do return $ isPut (unwrap request')
  cond (primary, m) \case
    True -> do
      -- relay the request to backup
      request'' <- (primary, request') ~> backup
      -- process the request
      ack <-
        backup `locally` \unwrap ->
          handleRequest (unwrap request'') (unwrap backupStateRef)
      -- send acknowledgement from backup to primary
      (backup, ack) ~> primary
      return ()
    False -> do
      return ()

  -- process request on primary
  response <-
    primary `locally` \unwrap ->
      handleRequest (unwrap request') (unwrap primaryStateRef)

  -- send response to client
  (primary, response) ~> client

mainChoreo :: Choreo IO ()
mainChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (IORef State @ "primary", IORef State @ "backup") -> Choreo IO ()
    loop stateRefs = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRefs
      client `locally` \unwrap -> do putStrLn (show (unwrap response))
      loop stateRefs

main :: IO ()
main = do
  runChoreo mainChoreo
