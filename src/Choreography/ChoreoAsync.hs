{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.ChoreoAsync where

import Control.Concurrent.Async
import Choreography.Location
import Choreography.NetworkAsync
import Control.Monad.Freer
import Data.List
import Data.Proxy
import GHC.TypeLits
import Control.Monad.IO.Class
import Control.Monad.State hiding (lift)
import Control.Monad.State qualified as S

----------------------------------------------------------------------
-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values
-- located at a specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that
-- represents local computations.
data ChoreoSig m a where
  Local :: (KnownSymbol l)
        => Proxy l
        -> (Unwrap l -> m a)
        -> ChoreoSig m (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> a @ l
       -> Proxy l'
       -> ChoreoSig m (Async a @ l')

-- | Monad for writing choreographies.
type Choreo m = Freer (ChoreoSig m)

----------------------------------------------------------------------
-- * Endpoint projection

epp :: (MonadIO m) => Choreo m a -> LocTm -> Network m a
epp c l' = evalStateT (interpFreer handler c) 0
  where
    handler :: (MonadIO m) => ChoreoSig m a -> StateT Int (Network m) a
    handler (Local l m)
      | toLocTm l == l' = S.lift (wrap <$> lift (m unwrap))
      | otherwise       = S.lift (return Empty)
    handler (Comm s a r)
      | toLocTm s == toLocTm r = inc >> S.lift (lift $ liftIO $ wrap <$> async (return (unwrap a)))
      | toLocTm s == l'        = inc >>= \n ->  S.lift (send (unwrap a) (toLocTm r) n >> return Empty)
      | toLocTm r == l'        = inc >>= \n -> S.lift (wrap <$> recv (toLocTm s) n)
      | otherwise              = inc >> S.lift (return Empty)

    inc :: (Monad m) => StateT Int m Int
    inc = do
      n <- get
      put (n + 1)
      return n

----------------------------------------------------------------------
-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo m (a @ l)
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l, a @ l)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> Choreo m (Async a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
