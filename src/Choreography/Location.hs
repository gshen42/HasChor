-- | This module defines locations and functions for working with them.
module Choreography.Location where

import Data.Kind  

-- | Type-level locations
type LocTy = Type

-- | Term-level locations
type LocTm = String
