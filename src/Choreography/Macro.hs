{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | This module is an attempt to define a macro-like function that performs
-- some action over a list of participants.
module Choreography.Macro where

import Choreography.Location
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

class TMap (ls :: [Type]) where
  forEach :: (Monad m) => (forall (l :: Type). (Typeable l) => Proxy l -> m ()) -> m ()

-- foo :: IO ()
-- foo = mapMT2 @[Int, Bool] callback
--   where
--     callback :: forall l. (Typeable l) => Proxy l -> IO ()
--     callback _ = print (reify @l) >> return ()

instance TMap '[] where
  forEach f = undefined

instance (Typeable x, TMap xs) => TMap (x ': xs) where
  forEach f = f (Proxy :: Proxy x) >> forEach @xs f
