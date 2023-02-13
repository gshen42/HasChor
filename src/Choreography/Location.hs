{-# LANGUAGE DataKinds #-}

module Choreography.Location where

import Data.String
import GHC.TypeLits

type LocTm = String
type LocTy = Symbol

-- a location is a string that lives both at the term-level and type-level
-- this can be trivially done in a dependently-typed language
-- we simulate this in Haskell as a datatype indexed by itself
-- `LocTm` is the term-level location and `LocTy` is the type-level string
-- and *it is the user's responsibility to ensure they are the same*
-- ^ TODO: make a template haskell function that guarantee this by construction
data Location (l :: LocTy) = Location LocTm

toLocTm :: Location l -> LocTm
toLocTm (Location l) = l

instance IsString (Location l) where
  fromString = Location

newtype a @ (l :: LocTy) = Wrap a

wrap :: a -> a @ l
wrap = Wrap

unwrap :: a @ l -> a
unwrap (Wrap a) = a
