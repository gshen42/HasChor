{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and related funcitons.
module Choreography.Location where

import Data.Kind (Constraint, Type)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Term-level locations
type LocTm = String

-- | Type-level locations
class LocTy l where
  reify :: LocTm

instance (Typeable l) => LocTy l where
  reify = show (typeRep (Proxy :: Proxy l))

instance (KnownSymbol l) => LocTy l where
  reify = symbolVal (Proxy :: Proxy l)

eqLoc :: forall a b. (LocTy a, LocTy b) => Bool
eqLoc = reify @a == reify @b

-- | Type-level list membership.
type family Member l ls :: Constraint where
  Member x (x : xs) = ()
  Member x (y : xs) = Member x xs

class LocTyList ls where
  reifyList :: [LocTm]

instance LocTyList [] where
  reifyList = []

instance (LocTy x, LocTyList xs) => LocTyList (x : xs) where
  reifyList = reify @x : reifyList @xs
