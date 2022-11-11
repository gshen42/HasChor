--------------------------------------------------------------------------------
-- Monads for writing choreographies.
--------------------------------------------------------------------------------
module Choreography.Choreo (Choreo, comm, epp) where

import Choreography.Control
import Choreography.Location
import Control.Concurrent.Async

data Choreo a where
    Pure :: a -> Choreo a
    Comm :: (Show x, Read x) => x -> Location -> Location -> (x -> Choreo a) -> Choreo a

instance Functor Choreo where
    fmap f (Pure a)       = Pure (f a)
    fmap f (Comm x s r k) = Comm x s r (fmap f . k)

instance Applicative Choreo where
    pure = Pure

    f <*> x = f >>= \f -> x >>= \x -> return (f x)

instance Monad Choreo where
    (Pure a)       >>= f = f a
    (Comm x s r k) >>= f = k x >>= f

runChoreo :: Choreo a -> a
runChoreo (Pure a)     = a
runChoreo (Comm x s r k) = runChoreo (k x)

comm :: (Show x, Read x) => x -> Location -> Location -> Choreo x
comm x s r = Comm x s r Pure

epp :: Choreo a -> Location -> Control a
epp (Comm x s r k) l
    | l == s    = send x r >> epp (k x) l
    | l == r    = recv s >>= \r -> epp (k r) l
    | otherwise = epp (k x) l
epp (Pure a) l = return a