{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | This module is the entry-point for HasChor. It re-exports all the necessary
-- types and funcitons for writing and executing choreographies.
module Choreography
  ( -- * Locations
    Loc,

    -- * Located values and compuations
    type (@),
    Located,
    unwrap,

    -- * Choreographies
    Choreo,
    ChoreoIO,
    locally,
    comm,
    cond,
    commFork,

    -- * Message transport backends

    -- * The HTTP backend
    Host,
    Port,
    HttpConfig,
    mkHttpConfig,

    -- * Running choreographies
  )
where

import Choreography.Choreo
import Choreography.Located
import Choreography.Location
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.IO.Class
import Data.Typeable

runChoreo :: forall l {cfg} {ps} {m} {a}. (Typeable l, Backend cfg, MonadIO m) => cfg -> Choreo ps m a -> m a
runChoreo cfg c = runNetwork cfg (reify @l) $ epp @l c
