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

import Choreography.Choreo
import Control.Monad.Tree
