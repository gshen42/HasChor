{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

--------------------------------------------------------------------------------
-- Monads for writing choreographies.
--------------------------------------------------------------------------------

module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer
import Data.List
import Data.Proxy
import GHC.TypeLits

type Unwrap l = forall a. a @ l -> a

data ChoreoSig m a where
  Local :: (KnownSymbol l) =>
           Proxy l -> (Unwrap l -> m a) -> ChoreoSig m (a @ l)

  Comm  :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
           Proxy l -> a @ l -> Proxy l' -> ChoreoSig m (a @ l')
  -- ^ TODO: ensure l and l' are different

  Cond  :: (Show a, Read a, KnownSymbol l) =>
           Proxy l -> a @ l -> (a -> Choreo m b) -> ChoreoSig m b

type Choreo m = Freer (ChoreoSig m)

runChoreo :: Monad m => Choreo m a -> m a
runChoreo = runFreer alg
  where
    alg :: Monad m => ChoreoSig m a -> m a
    alg (Local _ m)  = wrap <$> m unwrap
    alg (Comm _ a _) = return $ (wrap . unwrap) a
    alg (Cond _ a c) = runChoreo $ c (unwrap a)

-- TODO: use type family to precisely specify the return type of `Network`
-- TODO: is it possible to define `epp` in terms of `runFreer`
-- TODO: use a proper exception instead of `undefined` to indicate data ownership
epp :: Choreo m a -> LocTm -> Network m a
epp (Return a) l = return a
epp (Do (Local l m) k) l'
  | toLocTm l == l' = run (m unwrap) >>= \x -> epp (k $ wrap x) l'
  | otherwise       = epp (k undefined) l'
epp (Do (Comm s a r) k) l
  | toLocTm s == l = send (unwrap a) (toLocTm r) >> epp (k undefined) l
  | toLocTm r == l = recv (toLocTm s) >>= \x -> epp (k (wrap x)) l
  | otherwise      = epp (k undefined) l
epp (Do (Cond l a c) k) l'
  | toLocTm l == l' = broadcast (unwrap a) >> epp (c $ unwrap a) l' >>= \x -> epp (k x) l'
  | otherwise       = recv (toLocTm l) >>= \x -> epp (c x) l' >>= \x -> epp (k x) l'

  --------------------------------------------------------------------------------
  -- `Choreo` operators

locally :: KnownSymbol l =>
           Proxy l -> (Unwrap l -> m a) -> Choreo m (a @ l)
locally l m = toFreer (Local l m)

comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
        (Proxy l, a @ l) -> Proxy l' -> Choreo m (a @ l')
comm (l, a) l' = toFreer (Comm l a l')

(~>) (l, a) l' = (l, a) `comm` l'

(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
         (Proxy l, Unwrap l -> m a) -> Proxy l' -> Choreo m (a @ l')
(~~>) (l, m) l' = do
  x <- l `locally` m
  (l, x) ~> l'

cond :: (Show a, Read a, KnownSymbol l) =>
        (Proxy l, a @ l) -> (a -> Choreo m b) -> Choreo m b
cond (l, a) c = toFreer (Cond l a c)

cond' :: (Show a, Read a, KnownSymbol l) =>
         (Proxy l, Unwrap l -> m a) -> (a -> Choreo m b) -> Choreo m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c
