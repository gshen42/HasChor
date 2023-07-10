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

server :: Proxy "server"
server = Proxy

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

-- | `readRequest` reads a request from the terminal.
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

-- | `handleRequest` handle a request and returns the new the state.
handleRequest :: Request -> IORef State -> IO Response
handleRequest request stateRef = case request of
  Put key value -> do
    modifyIORef stateRef (Map.insert key value)
    return (Just value)
  Get key -> do
    state <- readIORef stateRef
    return (Map.lookup key state)

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
kvs ::
  Request @ "client" ->
  IORef State @ "server" ->
  Choreo IO (Response @ "client")
kvs request stateRef = do
  -- send the request to the server
  request' <- (client, request) ~> server
  -- the server handles the response and creates a response
  response <-
    server `locally` \unwrap ->
      handleRequest (unwrap request') (unwrap stateRef)
  -- send the response back to the client
  (server, response) ~> client

-- | `mainChoreo` is a choreography that serves as the entry point of the program.
-- It initializes the state and loops forever.
mainChoreo :: Choreo IO ()
mainChoreo = do
  stateRef <- server `locally` \_ -> newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: IORef State @ "server" -> Choreo IO ()
    loop stateRef = do
      request <- client `locally` \_ -> readRequest
      response <- kvs request stateRef
      client `locally` \unwrap -> do putStrLn ("> " ++ (show (unwrap response)))
      loop stateRef

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config mainChoreo "client"
    "server" -> runChoreography config mainChoreo "server"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("server", ("localhost", 4000))
        ]
