{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines locations, located values, and functions for working with them.
module Choreography.Location where

import Data.Kind
import Data.Typeable

-- * Locations

-- | Type-level locations
type Loc = Type

-- | Term-level locations
type LocTm = String

-- | Type-level list membership.
type family Member (l :: Loc) (ls :: [Loc]) :: Constraint where
  Member x (x : xs) = ()
  Member x (y : xs) = Member x xs

-- the following two functions require the AllowAmbiguousTypes extension
-- but in practice there's no ambiguity because this function is intended
-- to be used with visiable type applications

eqLoc :: forall (a :: Loc) (b :: Loc). (Typeable a, Typeable b) => Bool
eqLoc =
  let p1 = Proxy :: Proxy a
      p2 = Proxy :: Proxy b
   in typeRep p1 == typeRep p2

reify :: forall (a :: Loc). (Typeable a) => LocTm
reify = let p = (Proxy :: Proxy a) in show (typeRep p)

-- | Located values.
--
-- @a \@ l@ represents a value of type @a@ at location @l@.
data a @ (l :: Loc)
  = -- | A located value @a \@ l@ from location @l@'s perspective.
    Wrap a
  | -- | A located value @a \@ l@ from locations other than @l@'s
    -- perspective.
    Empty

-- | Wrap a value as a located value.
wrap :: a -> a @ l
wrap = Wrap

-- | Unwrap a located value.
--
-- /Note:/ Unwrapping a empty located value will throw an exception.
unsafeUnwrap :: a @ l -> a
unsafeUnwrap (Wrap a) = a
unsafeUnwrap Empty = error "this should never happen for a well-typed choreography"

class Unwrappable l where
  un :: a @ l -> a