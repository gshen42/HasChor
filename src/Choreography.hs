{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  mkLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  cond,
  par,

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
  mkHttpConfig,

  -- * Running choreographies
  -- runChoreo,
  runChoreography,
  ) where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.Tree
import Control.Monad.Reader

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config) => config -> Choreo IO a -> LocTm -> IO a
runChoreography cfg c l = runNetwork cfg l (runReaderT (unEpp (epp c l)) Root)
