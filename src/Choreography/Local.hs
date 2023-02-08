{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- Monads for local computations at a location in a choreography.
--------------------------------------------------------------------------------

module Choreography.Local where

import Choreography.Location
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)

newtype Local (l :: Symbol) a = Local (IO a)

unLocal :: Local l a -> IO a
unLocal (Local x) = x

runLocal = unLocal

instance Functor (Local l) where
  fmap f = Local . fmap f . unLocal

instance Applicative (Local l) where
  pure = Local . pure

  f <*> a = Local $ (unLocal f) <*> (unLocal a)

instance Monad (Local l) where
  m >>= f = Local $ (unLocal m) >>= (unLocal . f)

instance MonadIO (Local l) where
  liftIO = Local

-- related: https://stackoverflow.com/questions/53801780/wrong-kind-in-symbolval-to-get-a-type-level-string-at-runtime
loc :: forall l a . KnownSymbol l => Local l a -> Location
loc _ = Location $ symbolVal (Proxy :: Proxy l)
