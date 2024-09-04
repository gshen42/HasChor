module Main where

import Control.Monad
import Control.Monad.Async
import Data.ByteString as BS
import Data.String
import Network.Simple.TCP as Net
import Control.Monad.IO.Class

-- | A test for the `Async` monad.
asyncTest :: IO ()
asyncTest = runAsync $ do
  _ <-
    (,)
      <$> (liftIO (receive (Host "localhost") "8000") >>= liftIO . print)
      <*> (liftIO (receive (Host "localhost") "8080") >>= liftIO . print)
  return ()
  where
    receive :: HostPreference -> ServiceName -> IO StrictByteString
    receive hp sn = do
      listen hp sn $ \(lsock, lsockAddr) -> do
        print ("listen", lsock, lsockAddr)
        accept lsock $ \(sock, sockAddr) -> do
          print ("accept", sock, sockAddr)
          bytes <- recv sock 4096
          maybe (error "closed") return bytes

main :: IO ()
main = asyncTest