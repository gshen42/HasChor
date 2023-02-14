{-# LANGUAGE DataKinds #-}

module Choreography.Location where

import Data.Proxy
import Data.String
import GHC.TypeLits

type LocTm = String
type LocTy = Symbol

toLocTm :: KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

data (l :: LocTy) . a = Wrap a

type a @ l = l.a

instance Functor ((.) l) where
  fmap f (Wrap a) = Wrap $ f a

instance Applicative ((.) l) where
  pure = Wrap

  (Wrap f) <*> (Wrap a) = Wrap $ f a

instance Monad ((.) l) where
  (Wrap a) >>= f = f a

wrap :: a -> l.a
wrap = Wrap

unwrap :: l.a -> a
unwrap (Wrap a) = a
