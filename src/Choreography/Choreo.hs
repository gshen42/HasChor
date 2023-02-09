{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

--------------------------------------------------------------------------------
-- Monads for writing choreographies.
--------------------------------------------------------------------------------

module Choreography.Choreo where

import Choreography.Location
import Control.Monad.Freer
import Data.Proxy
import GHC.TypeLits

newtype a @ (l :: Symbol) = Wrap a

wrap :: a -> a @ l
wrap = Wrap

unwrap :: a @ l -> a
unwrap (Wrap a) = a

type Unwrap l = forall a. a @ l -> a

-- I really want to put the constraint (Monad m) in the definition, but that
-- needs -XDatatypeContexts and is considered a bad practice, why? also it
-- doens't work with GADT
data ChoreoSig m a where
  Local :: Proxy l -> (Unwrap l -> m a) -> ChoreoSig m (a @ l)
  Comm  :: Proxy l -> a @ l -> Proxy l' -> ChoreoSig m (a @ l')
  -- ^ TODO: ensure l and l' are different

type Choreo m = Freer (ChoreoSig m)

locallyDo :: Proxy l -> (Unwrap l -> m a) -> Choreo m (a @ l)
locallyDo l m = toFreer (Local l m)

(~>) :: (Proxy l, a @ l) -> Proxy l' -> Choreo m (a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

runChoreo :: Monad m => Choreo m a -> m a
runChoreo = runFreer alg
  where
    alg :: Monad m => ChoreoSig m a -> m a
    alg (Local _ m)   = wrap <$> m unwrap
    alg (Comm _ a _ ) = return $ (wrap . unwrap) a

-- epp :: Choreo a -> Location -> Control a
-- epp (Comm x s r k) l
--     | l == s    = send x r >> epp (k x) l
--     | l == r    = recv s >>= \r -> epp (k r) l
--     | otherwise = epp (k x) l
-- epp (Pure a) l = return a
