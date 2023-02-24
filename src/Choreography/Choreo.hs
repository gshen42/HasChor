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
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig m a -> m a
    handler (Local _ m)  = wrap <$> m unwrap
    handler (Comm _ a _) = return $ (wrap . unwrap) a
    handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- TODO: use type family to precisely specify the return type of `Network`
-- TODO: is it possible to define `epp` in terms of `runFreer`
epp :: Choreo m a -> LocTm -> Network m a
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig m a -> Network m a
    handler (Local l m)
      | toLocTm l == l' = run (m unwrap) >>= return . wrap
      | otherwise       = return Empty
    handler (Comm s a r)
      | toLocTm s == l' = send (unwrap a) (toLocTm r) >> return Empty
      | toLocTm r == l' = recv (toLocTm s) >>= return . wrap
      | otherwise       = return Empty
    handler (Cond l a c)
      | toLocTm l == l' = broadcast (unwrap a) >> epp (c (unwrap a)) l'
      | otherwise       = recv (toLocTm l) >>= \x -> epp (c x) l'

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
