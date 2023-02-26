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

-- | ReplicationStrategy specifies how a request should be handled on possibly replicated servers
-- `a` is a type that represent states across locations
type ReplicationStrategy a =
  Request @ "primary" -> a -> Choreo IO (Response @ "primary")

nullReplicationStrategy :: ReplicationStrategy (IORef State @ "primary")
nullReplicationStrategy request stateRef = do
  primary `locally` \unwrap ->
    handleRequest (unwrap request) (unwrap stateRef)

primaryBackupReplicationStrategy ::
  ReplicationStrategy (IORef State @ "primary", IORef State @ "backup")
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  m <- primary `locally` \unwrap -> do return $ isPut (unwrap request)
  cond (primary, m) \case
    True -> do
      request'' <- (primary, request) ~> backup
      ( backup,
        \unwrap ->
          handleRequest (unwrap request'') (unwrap backupStateRef)
        )
        ~~> primary
      return ()
    False -> do
      return ()

  -- process request on primary
  primary `locally` \unwrap ->
    handleRequest (unwrap request) (unwrap primaryStateRef)

kvs ::
  forall a.
  Request @ "client" ->
  a ->
  ReplicationStrategy a ->
  Choreo IO (Response @ "client")
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client

nullReplicationChoreo :: Choreo IO ()
nullReplicationChoreo = do
  stateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: IORef State @ "primary" -> Choreo IO ()
    loop stateRef = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRef nullReplicationStrategy
      client `locally` \unwrap -> do putStrLn (show (unwrap response))
      loop stateRef

primaryBackupChoreo :: Choreo IO ()
primaryBackupChoreo = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup `locally` \_ -> newIORef (Map.empty :: State)
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (IORef State @ "primary", IORef State @ "backup") -> Choreo IO ()
    loop stateRefs = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      client `locally` \unwrap -> do putStrLn ("> " ++ show (unwrap response))
      loop stateRefs

main :: IO ()
main = do
  runChoreo primaryBackupChoreo
