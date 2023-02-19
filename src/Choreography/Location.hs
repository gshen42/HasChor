{-# LANGUAGE DataKinds #-}

module Choreography.Location where

import Data.Proxy
import Data.String
import GHC.TypeLits

type LocTm = String
type LocTy = Symbol

toLocTm :: KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

data a @ (l :: LocTy) = Wrap a

wrap :: a -> a @ l
wrap = Wrap

unwrap :: a @ l-> a
unwrap (Wrap a) = a
