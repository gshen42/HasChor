{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Reader
import Control.Monad.Tree
import Data.Binary
import Data.Proxy
import GHC.TypeLits

-- | A constrained version of `unwrap` that only unwraps values located at a specific
-- location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents local computations.
data ChoreoSig m a where
  Locally :: (KnownSymbol l) => Proxy l -> (Unwrap l -> m a) -> ChoreoSig m (a @ l)
  Comm :: (Binary a, KnownSymbol l, KnownSymbol l') =>
    Proxy l -> a @ l -> Proxy l' -> ChoreoSig m (a @ l')
  Cond :: (Binary a, KnownSymbol l) => Proxy l -> a @ l -> (a -> Choreo m b) -> ChoreoSig m b

-- | Monad for choreographies.
type Choreo m a = Tree (ChoreoSig m) a

-- | Perform a local computation at a given location.
locally :: (KnownSymbol l) => Proxy l -> (Unwrap l -> m a) -> Choreo m (a @ l)
locally l a = Perf (Locally l a)

-- | Communication between a sender and a receiver.
(~>) :: (Binary a, KnownSymbol l, KnownSymbol l') => a @ l -> Proxy l' -> Choreo m (a @ l')
a ~> l' = Perf (Comm (Proxy :: Proxy l) a l')

-- TODO: figure out why `Unwrap l -> m a` requires `ImpredicativeTypes` and is this safe
-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Binary a, KnownSymbol l, KnownSymbol l') =>
  (Proxy l, Unwrap l -> m a) -> Proxy l' -> Choreo m (a @ l')
(l, a) ~~> l' = do
  x <- l `locally` a
  x ~> l'

-- | Conditionally execute choreographies based on a located value.
cond :: (Binary a, KnownSymbol l) => a @ l -> (a -> Choreo m b) -> Choreo m b
cond a f = Perf (Cond (Proxy :: Proxy l) a f)

-- Since endpoint projection generates fresh session ids along the way, it needs to keep track of
-- the current id, making epp itself effectful. Here, the effect we want is reading a value from an
-- environment, and we use the reader monad transformer on top of the network monad to achieve that.
newtype Epp m a = Epp { unEpp :: ReaderT SessionId (Network m) a }
  deriving (Functor, Monad, MonadReader SessionId)

instance Applicative (Epp m) where
  pure = Epp . pure
  f <*> a = Epp $ local (Nest (Left ())) (unEpp f) <*> local (Nest (Right ())) (unEpp a)

-- TODO: why this can't be automatically derived?
instance MonadTrans Epp where
  lift = Epp . lift . exec

-- | Run a `Choreo` monad directly.
-- runChoreo :: Monad m => Choreo m a -> m a
-- runChoreo = interpFreer handler
--   where
--     handler :: Monad m => ChoreoSig m a -> m a
--     handler (Local _ m)  = wrap <$> m unwrap
--     handler (Comm _ a _) = return $ (wrap . unwrap) a
--     handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- | Endpoint projection.
epp :: Choreo m a -> LocTm -> Epp m a
epp c l' = interp handler c
  where
    handler :: ChoreoSig m a -> Epp m a
    handler (Locally l m)
      | toLocTm l == l' = Epp $ wrap <$> lift (exec (m unwrap))
      | otherwise       = return Empty
    handler (Comm s a r)
      | toLocTm s == toLocTm r = return $ wrap (unwrap a)
      | toLocTm s == l' = Epp $ do
        sid <- ask
        lift (send sid (unwrap a) (toLocTm r) >> return Empty)
      | toLocTm r == l' = Epp $ do
        sid <- ask
        lift (wrap <$> recv sid (toLocTm s))
      | otherwise = return Empty
    handler (Cond l a k)
      | toLocTm l == l' =
        Epp (do sid <- ask; lift $ broadcast sid (unwrap a)) >> epp (k (unwrap a)) l'
      | otherwise =
        Epp (do sid <- ask; lift $ recv sid (toLocTm l)) >>= \x -> epp (k x) l'
