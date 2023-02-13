{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- Monads for writing choreographies.
--------------------------------------------------------------------------------

module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer

type Unwrap l = forall a. a @ l -> a

-- I really want to put the constraint (Monad m) in the definition, but that
-- needs -XDatatypeContexts and is considered a bad practice, why? also it
-- doens't work with GADT
data ChoreoSig m a where
  Local :: Location l -> (Unwrap l -> m a) -> ChoreoSig m (a @ l)
  Comm  :: (Show a, Read a) => Location l -> a @ l -> Location l' -> ChoreoSig m (a @ l')
  -- ^ TODO: ensure l and l' are different

type Choreo m = Freer (ChoreoSig m)

locallyDo :: Location l -> (Unwrap l -> m a) -> Choreo m (a @ l)
locallyDo l m = toFreer (Local l m)

(~>) :: (Show a, Read a) => (Location l, a @ l) -> Location l' -> Choreo m (a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

runChoreo :: Monad m => Choreo m a -> m a
runChoreo = runFreer alg
  where
    alg :: Monad m => ChoreoSig m a -> m a
    alg (Local _ m)   = wrap <$> m unwrap
    alg (Comm _ a _ ) = return $ (wrap . unwrap) a

-- TODO: use type family to precisely specify the return type of `Network`
-- TODO: is it possible to define `epp` in terms of `runFreer`
-- TODO: use a proper exception instead of `undefined` to indicate data ownership
epp :: Choreo m a -> Location l -> Network m ()
epp (Return a) l = return ()
epp (Do (Local l m) k) l'
  | toLocTm l == toLocTm l' = loca (m unwrap) >>= \x -> epp (k (wrap x)) l
  | otherwise               = epp (k undefined) l
epp (Do (Comm s a r) k) l
  | toLocTm s == toLocTm l = send (unwrap a) r >> epp (k undefined) l
  | toLocTm r == toLocTm l = recv s >>= \x -> epp (k (wrap x)) l
  | otherwise              = epp (k undefined) l
