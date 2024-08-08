{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.ChoreoAsync where

import Choreography.Location
import Choreography.NetworkAsync
import Control.Concurrent.Async
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.State hiding (lift)
import Control.Monad.State qualified as S
import Data.List
import Data.Proxy
import GHC.TypeLits

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
data ChoreoSig m a where
  Local ::
    (KnownSymbol l) =>
    Proxy l ->
    (Unwrap l -> m a) ->
    ChoreoSig m (a @ l)
  Comm ::
    (KnownSymbol l, KnownSymbol l', Show a, Read a) =>
    Proxy l ->
    a @ l ->
    Proxy l' ->
    ChoreoSig m (Async a @ l')
  Cond ::
    (KnownSymbol l, Show a, Read a) =>
    Proxy l ->
    a @ l ->
    (a -> Choreo m b) ->
    ChoreoSig m b

-- | Monad for writing choreographies.
type Choreo m = Freer (ChoreoSig m)

-- | Run a `Choreo` monad directly.
-- runChoreo :: (Monad m) => Choreo m a -> m a
-- runChoreo = interpFreer handler
--   where
--     handler :: (Monad m) => ChoreoSig m a -> m a
--     handler (Local _ m) = wrap <$> m unwrap
--     handler (Comm _ a _) = return $ (wrap . unwrap) a
--     handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- | Perform a local computation at a given location.
locally ::
  (KnownSymbol l) =>
  -- | Location performing the local computation.
  Proxy l ->
  -- | The local computation given a constrained unwrap funciton.
  (Unwrap l -> m a) ->
  Choreo m (a @ l)
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) ::
  (Show a, Read a, KnownSymbol l, KnownSymbol l') =>
  -- | A pair of a sender's location and a value located at the sender
  (Proxy l, a @ l) ->
  -- | A receiver's location.
  Proxy l' ->
  Choreo m (Async a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

-- | Conditionally execute choreographies based on a located value.
cond ::
  (Show a, Read a, KnownSymbol l) =>
  -- | A pair of a location and a scrutinee located on
  -- it.
  (Proxy l, a @ l) ->
  -- | A function that describes the follow-up
  -- choreographies based on the value of scrutinee.
  (a -> Choreo m b) ->
  Choreo m b
cond (l, a) c = toFreer (Cond l a c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) ::
  (KnownSymbol l, KnownSymbol l', Show a, Read a) =>
  -- | A pair of a sender's location and a local computation.
  (Proxy l, Unwrap l -> m a) ->
  -- | A receiver's location.
  Proxy l' ->
  Choreo m (Async a @ l')
(~~>) (l, m) l' = do
  x <- l `locally` m
  (l, x) ~> l'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' ::
  (Show a, Read a, KnownSymbol l) =>
  -- | A pair of a location and a local computation.
  (Proxy l, Unwrap l -> m a) ->
  -- | A function that describes the follow-up choreographies based on the
  -- result of the local computation.
  (a -> Choreo m b) ->
  Choreo m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c

class HasFail a where
  failVal :: a

instance HasFail Int where
  failVal = -1

select :: (KnownSymbol l, Eq a, HasFail a) => Proxy l -> (Async a @ l) -> (Async a @ l) -> Choreo IO (Async a @ l)
select l x y = do
  l `locally` \un -> do
    x' <- poll (un x)
    case x' of
      (Just (Right a)) | a /= failVal -> return $ un x
      _ -> do
        y' <- poll (un y)
        case y' of
          (Just (Right _)) -> return $ un y
          _ -> async (return failVal)

-- * Endpoint projection

epp :: (MonadIO m) => Choreo m a -> LocTm -> Network m a
epp c l' = evalStateT (interpFreer handler c) 0
  where
    handler :: (MonadIO m) => ChoreoSig m a -> StateT Int (Network m) a
    handler (Local l m)
      | toLocTm l == l' = S.lift (wrap <$> run (m unwrap))
      | otherwise = S.lift (return Empty)
    handler (Comm s a r)
      | toLocTm s == toLocTm r = inc >> S.lift (run $ liftIO $ wrap <$> async (return (unwrap a)))
      | toLocTm s == l' = inc >>= \n -> S.lift (send (unwrap a) (toLocTm r) n >> return Empty)
      | toLocTm r == l' = inc >>= \n -> S.lift (wrap <$> recv (toLocTm s) n)
      | otherwise = inc >> S.lift (return Empty)
    handler (Cond l a c)
      | toLocTm l == l' = do
          n <- inc
          S.lift $ broadcast (unwrap a) n
          S.lift $ epp (c (unwrap a)) l'
      | otherwise = do
          n <- inc
          m <- S.lift $ recv (toLocTm l) n
          x <- S.lift $ run $ liftIO $ wait m
          S.lift $ epp (c x) l'

    inc :: (Monad m) => StateT Int m Int
    inc = do
      n <- get
      put (n + 1)
      return n
