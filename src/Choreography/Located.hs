module Choreography.Located where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype a @ l = Wrap {unsafeUnwrap :: a}

wrap :: a -> a @ l
wrap = Wrap

newtype Located l m a = Located {runLocated :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Located l) where
  lift = Located

unwrap :: (Monad m) => a @ l -> Located l m a
unwrap = return . unsafeUnwrap
