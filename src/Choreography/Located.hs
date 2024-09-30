module Choreography.Located where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))

-- | A value of type `a` located at `l`.
data a @ l = Wrap a | Empty

-- | Unsafely unwrap a located value and raise an error if it's empty.
unsafeUnwrap :: a @ l -> a
unsafeUnwrap (Wrap a) = a
unsafeUnwrap Empty = error "Internal Error: attemp to access a empty located value!"

-- | A computation located at `l`.
newtype Located l m a = Located {unLocated :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Located IO computations.
type LocatedIO l a = Located l IO a

instance MonadTrans (Located l) where
  lift = Located

-- | Unwrap a value at `l` to use in a compuation at the same locaiton.
unwrap :: (Monad m) => a @ l -> Located l m a
unwrap = return . unsafeUnwrap
