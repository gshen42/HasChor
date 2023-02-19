{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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

site1 :: Proxy "site1"
site1 = Proxy

site2 :: Proxy "site2"
site2 = Proxy

site3 :: Proxy "site3"
site3 = Proxy

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

parseRequest :: String -> Maybe Request
parseRequest s =
  let l = words s
   in case l of
        ["GET", k] -> Just (Get k)
        ["PUT", k, v] -> Just (Put k v)
        _ -> Nothing

readRequest :: KnownSymbol a => Proxy a -> Choreo IO (Request @ a)
readRequest a = do
  a `locallyDo` \unwrap -> do
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

handleRequest :: KnownSymbol a => Proxy a -> State @ a -> Request @ a -> Choreo IO ((State, Response) @ a)
handleRequest a state request = do
  a `locallyDo` \unwrap -> case unwrap request of
    Put k v -> do
      return (Map.insert k v (unwrap state), Just "OK")
    Get k -> do
      let e = Map.lookup k (unwrap state)
       in do
            return (unwrap state, e)

kvs ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  State @ a ->
  Request @ a ->
  (Request @ b -> Choreo IO c) ->
  Choreo IO ((State, Response) @ a, c)
kvs a b state request next = do
  tmp <- handleRequest a state request
  request' <- (a, request) ~> b
  c <- next request'
  return (tmp, c)

xfst :: KnownSymbol a => Proxy a -> (b, c) @ a -> Choreo IO (b @ a)
xfst a t = do
  a `locallyDo` \unwrap -> do return $ fst (unwrap t)

xsnd :: KnownSymbol a => Proxy a -> (b, c) @ a -> Choreo IO (c @ a)
xsnd a t = do
  a `locallyDo` \unwrap -> do return $ snd (unwrap t)

initKvs :: KnownSymbol a => Proxy a -> Choreo IO (State @ a)
initKvs a = do
  a `locallyDo` \unwrap -> return (Map.empty :: State)

mainChoreo :: Choreo IO ()
mainChoreo = do
  i1 <- initKvs site1
  i2 <- initKvs site2
  i3 <- initKvs site3
  let loop (s1 :: State @ "site1", s2 :: State @ "site2", s3 :: State @ "site3") = do
        req <- readRequest client
        req1 <- (client, req) ~> site1
        (r1, (r2, (r3, ()))) <-
          kvs
            site1
            site2
            s1
            req1
            ( \req2 ->
                kvs
                  site2
                  site3
                  s2
                  req2
                  (\req3 -> kvs site3 site3 s3 req3 \_ -> do return ())
            )
        s1' <- xfst site1 r1
        s2' <- xfst site2 r2
        s3' <- xfst site3 r3

        res <- xsnd site1 r1
        res' <- (site1, res) ~> client
        client `locallyDo` \unwrap -> do print (unwrap res')
        site1 `locallyDo` \unwrap -> do print (unwrap s1')
        site2 `locallyDo` \unwrap -> do print (unwrap s2')
        site3 `locallyDo` \unwrap -> do print (unwrap s3')
        loop (s1', s2', s3')
   in loop (i1, i2, i3)
  return ()

main :: IO ()
main = do
  runChoreo mainChoreo
