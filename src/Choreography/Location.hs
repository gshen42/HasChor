{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and related funcitons.
module Choreography.Location where

import Data.Kind (Constraint, Type)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)

-- | Type-level locations.
--
-- Use visiable type applications or `Proxy`s to refer to them.
type Loc = Type

-- | Term-level locatinos.
type LocTm = String

-- | Convert a type-level location to a term-level one.
reify :: forall l. (Typeable l) => LocTm
reify = show (typeRep (Proxy :: Proxy l))

-- | Comparision between type-level locations.
eqLoc :: forall a b. (Typeable a, Typeable b) => Bool
eqLoc = reify @a == reify @b

-- | Type-level list membership.
type family Member l ls :: Constraint where
  Member x (x : xs) = ()
  Member x (y : xs) = Member x xs

-- | A list whose elements are all `Typeable`.
class TypeableList ls where
  reifyList :: [LocTm]

instance TypeableList [] where
  reifyList = []

instance (Typeable x, TypeableList xs) => TypeableList (x : xs) where
  reifyList = reify @x : reifyList @xs
