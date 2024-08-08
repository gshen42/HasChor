{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- recommended to only use constructs exported by this module.
module ChoreographyAsync
  ( -- * Locations and Located Values
    LocTm,
    LocTy,
    type (@),
    mkLoc,

    -- * The Choreo monad
    Choreo,
    locally,
    (~>),

    -- * Message transport backends

    -- ** The HTTP backend
    Host,
    Port,
    HttpConfig,
    mkHttpConfig,

    -- * Running choreographies
    runChoreography,
  )
where

import Choreography.ChoreoAsync
import Choreography.Location
import Choreography.NetworkAsync
import Choreography.NetworkAsync.Http
import Control.Monad.IO.Class
import Data.Proxy

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
