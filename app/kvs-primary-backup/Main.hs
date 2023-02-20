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
import GHC.TypeLits (KnownSymbol)
import System.Environment
import System.Exit (exitSuccess)

client :: Proxy "client"
client = Proxy

primary :: Proxy "primary"
primary = Proxy

backup :: Proxy "backup"
backup = Proxy

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

parseRequest :: String -> Maybe Request
parseRequest s =
  let l = words s
   in case l of
        ["GET", k] -> Just (Get k)
        ["PUT", k, v] -> Just (Put k v)
        _ -> Nothing

readCommand :: Choreo IO (Request @ "client")
readCommand = do
  client `locally` \unwrap -> do
    let loop = do
          putStrLn "Command?"
          line <- getLine
          let x = parseRequest line
           in case x of
                Just t -> return t
                Nothing -> do
                  putStrLn "Invalid command"
                  loop
    loop

handleRequest ::
  KnownSymbol s =>
  Proxy s ->
  KnownSymbol c =>
  Proxy c ->
  State @ s ->
  Request @ s ->
  Choreo IO (State @ s, Maybe String @ c)
handleRequest s c state req = do
  result <-
    s `locally` \unwrap -> case unwrap req of
      Put k v -> do
        return (Map.insert k v (unwrap state), Just "OK")
      Get k -> do
        let e = Map.lookup k (unwrap state)
         in do
              return (unwrap state, e)
  state' <- s `locally` \unwrap -> do return $ fst (unwrap result)
  msg <- s `locally` \unwrap -> do return $ snd (unwrap result)
  msg' <- (s, msg) ~> c
  return (state', msg')

kvs :: (State @ "primary", State @ "backup") -> Choreo IO (State @ "primary", State @ "backup")
kvs (sp, sb) = do
  req <- readCommand
  req' <- (client, req) ~> primary
  (sp', msg) <- handleRequest primary client sp req'
  client `locally` \unwrap -> do
    print $ show (unwrap msg)
  req'' <- (primary, req') ~> backup
  (sb', _) <- handleRequest backup primary sb req''
  primary `locally` \unwrap -> do
    print $ show (unwrap sp')
  backup `locally` \unwrap -> do
    print $ show (unwrap sb')
  kvs (sp', sb')

startKvs :: Choreo IO ()
startKvs = do
  initSp <- primary `locally` \unwrap -> return (Map.empty :: State)
  initSb <- backup `locally` \unwrap -> return (Map.empty :: State)
  kvs (initSp, initSb)
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  x <- case loc of
    "client" -> runChoreography config startKvs "client"
    "primary" -> runChoreography config startKvs "primary"
    "backup" -> runChoreography config startKvs "backup"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 5000)),
          ("primary", ("localhost", 5001)),
          ("backup", ("localhost", 5002))
        ]
