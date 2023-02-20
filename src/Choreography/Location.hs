{-# LANGUAGE DataKinds #-}

module Choreography.Location where

import Data.Proxy
import Data.String
import GHC.TypeLits

type LocTm = String
type LocTy = Symbol

toLocTm :: forall (l :: LocTy). KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

data a @ (l :: LocTy) = Wrap a
                      | Empty

wrap :: a -> a @ l
wrap = Wrap

unwrap :: a @ l-> a
unwrap (Wrap a) = a
unwrap Empty    = error "this should never happen for a well-typed choreography"
