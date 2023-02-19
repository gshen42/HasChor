{-# LANGUAGE ExplicitNamespaces #-}

--------------------------------------------------------------------------------
-- This module defines the interface to HasChor, the client of the library is
-- highly recommended to only use constructs exported by this module.
--------------------------------------------------------------------------------

module Choreography
  ( LocTm
  , LocTy
  , type (@)

  , Proxy(..)

  , Choreo
  , locallyDo
  , (~>)
  , cond
  , runChoreo

  , Host
  , Port
  , HttpConfig
  , mkHttpConfig

  , runChoreography
  )
  where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.IO.Class
import Data.Proxy

runChoreography :: (Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
