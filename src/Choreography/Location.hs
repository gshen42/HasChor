{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations and related funcitons.
module Choreography.Location where

import Data.Kind
import Data.Typeable

-- | Type-level locations
type Loc = Type

-- | Term-level locations
type LocTm = String

-- | Type-level list membership.
type family Member (l :: Loc) (ls :: [Loc]) :: Constraint where
  Member x (x : xs) = ()
  Member x (y : xs) = Member x xs

-- the following two functions require the AllowAmbiguousTypes extension
-- but in practice there's no ambiguity because they are intended
-- to be used with visiable type applications

eqLoc :: forall (a :: Loc) (b :: Loc). (Typeable a, Typeable b) => Bool
eqLoc =
  let p1 = Proxy :: Proxy a
      p2 = Proxy :: Proxy b
   in typeRep p1 == typeRep p2

reify :: forall (a :: Loc). (Typeable a) => LocTm
reify = let p = (Proxy :: Proxy a) in show (typeRep p)
