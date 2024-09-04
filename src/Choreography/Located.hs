-- | This module defines located computations, which add location modality to a
-- underlying computation.
module Choreography.Located where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

-- | Computations annnotaed with a location as a modality.
--
-- A `Located l m a` is essentially a `m (Maybe a)` with a phantom type `l`.
-- The @Maybe@ is a @Just@ if the location owns the data; otherwise, it is
-- a @Nothing@.
newtype Located l m a = Located {unLocated :: MaybeT m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Lift a underlying computation to a located computation.
present :: (Monad m) => m a -> Located l m a
present = Located . MaybeT . fmap Just

-- | Create a dummy located value.
absent :: (Monad m) => Located l m a
absent = Located . MaybeT $ return Nothing

-- | Unwrap a located value (/INTERNAL USE ONLY/).
unwrap :: (Monad m) => Located l m a -> m a
unwrap = fmap (fromMaybe $ error "unwrap error") . runMaybeT . unLocated
