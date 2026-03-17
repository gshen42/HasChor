{-# LANGUAGE ExplicitNamespaces #-}

module Choreography (
    type (@),
    Choreo,
    locally,
    (~>),
    (~~>),
    cond,
    par,
  ) where

import Choreography.Location
import Choreography.Choreo
import Control.Monad.Tree
