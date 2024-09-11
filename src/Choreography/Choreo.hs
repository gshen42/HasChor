{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Choreography.Choreo where

import Choreography.Located
import Choreography.Network
import Control.Concurrent.Future
import Control.Monad.Freer
import Data.Kind
import Data.Proxy

-- | Type-level list membership.
type family Member (l :: Type) (ls :: [Type]) :: Constraint where
  Member x (x : xs) = ()
  Member x (y : xs) = Member x xs

-- | Signature of the `Choreo` monad.
data ChoreoSig ls m a where
  Comm ::
    (Member s ls, Member r ls, Show a, Read a) =>
    Located s m a ->
    ChoreoSig ls m (Located r m (Future a))
  Cond ::
    (Member s ls) =>
    Located s m a ->
    (a -> Choreo ls m b) ->
    ChoreoSig ls m b
  -- equivalent to `Comm` to oneself except the data doesn't need to be serializable
  Locally :: (Member l ls) => Located l m a -> ChoreoSig ls m a

-- | The monad for choreographies.
type Choreo ls m a = Freer (ChoreoSig ls m) a

epp :: Choreo ls m a -> Proxy l -> Network m a
epp = undefined
