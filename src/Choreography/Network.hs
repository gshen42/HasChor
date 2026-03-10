module Choreography.Network where

import Control.Monad.Tree
import Data.Binary
import GHC.TypeLits.Singletons

type SessionId = ()

data NetworkSig m a where
  Exec :: m a -> NetworkSig m a
  Send :: Binary a => SessionId -> a -> SSymbol l -> NetworkSig m ()
  Recv :: Binary a => SessionId -> SSymbol l-> NetworkSig m a
  BCast :: Binary a => SessionId -> a -> NetworkSig m ()

type Network m = Tree (NetworkSig m)

exec :: m a -> Network m a
exec m = Perf (Exec m)

send :: Binary a => SessionId -> a -> SSymbol l -> Network m ()
send sid a l = Perf (Send sid a l)

recv :: Binary a => SessionId -> SSymbol l -> Network m a
recv sid l = Perf (Recv sid l)

broadcast :: Binary a => SessionId -> a -> Network m ()
broadcast sid a = Perf (BCast sid a)

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
-- class Backend c where
--   runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a
