{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- Monads for computations on a single process in a choreography.
--------------------------------------------------------------------------------

module Choreography.Process where

import Choreography.Location
import Control.Monad.IO.Class
import Data.Function

newtype Process (l :: Location) a = Process (IO a)

unProcess :: Process l a -> IO a
unProcess (Process x) = x

runProcess = unProcess

instance Functor (Process l) where
  fmap f = Process . fmap f . unProcess

instance Applicative (Process l) where
  pure = Process . pure

  f <*> a = Process $ (unProcess f) <*> (unProcess a)

instance Monad (Process l) where
  m >>= f = Process $ (unProcess m) >>= (unProcess . f)

instance MonadIO (Process l) where
  liftIO = Process
