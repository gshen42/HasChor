-- | This module is the entry-point for the library. It re-exports all the
-- user-facing types and functions from other modules.
module Choreography
  ( -- * Choreographies
    Choreo,
    comm,
    cond,
    locally,

    -- * Locations
    Loc (..),
    Proxy (..),

    -- * Located computations
    Located,

    -- * Asynchronous computations
    Async,
  )
where

import Choreography.Choreo
import Choreography.Located
import Choreography.Location
import Control.Monad.Async
import Data.Proxy
