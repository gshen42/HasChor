-- | A very basic futures library built on top of `MVar`s.
module Control.Concurrent.Future where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

newtype Future a = Future {unFuture :: MVar a}

wait :: Future a -> IO a
wait = takeMVar . unFuture

waitThen :: (MonadIO m) => Future a -> (a -> m b) -> m b
waitThen fut f = do
    a <- liftIO $ wait fut
    f a
