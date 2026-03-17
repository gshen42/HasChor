module Choreography.Network where

import Choreography.Location
import Control.Monad.Tree
import Data.Binary

-- We use `SessionId` to assign unique identifiers to two branches of a `App`.
-- The assignment algorithm works as the follows:
-- * The top-most choreography starts with id `Root`.
-- * Everytime we see a `App`, we assign the first branch of it id `Nest (Left ()) sid`, where `sid`
--   is the id of the current choreography. For the second branch, we use `Right ()` instead.
-- Two session ids are equal if they're structurally equal.
--
-- We could serialize a session id to a list of integers:
-- * `Root` is the empty list []
-- * Each `Nest` appends `[0]` (the left branch) or `[1]` (the right branch) to the current list.
-- For example, `Nest (Left ()) Root` corresponds to [0]; `Nest (Right ()) (Nest (Left ()) Root)`
-- corresponds to `[0, 1]`
data SessionId where
  Root :: SessionId
  Nest :: Either () () -> SessionId -> SessionId

instance Eq SessionId where
  Root == Root = True
  (Nest (Left ()) sid) == (Nest (Right ()) sid') = sid == sid'
  (Nest (Right ()) sid) == (Nest (Right ()) sid') = sid == sid'
  _ == _ = False

data NetworkSig m a where
  Exec :: m a -> NetworkSig m a
  Send :: Binary a => SessionId -> a -> LocTm -> NetworkSig m ()
  Recv :: Binary a => SessionId -> LocTm-> NetworkSig m a
  BCast :: Binary a => SessionId -> a -> NetworkSig m ()

type Network m = Tree (NetworkSig m)

exec :: m a -> Network m a
exec m = Perf (Exec m)

send :: Binary a => SessionId -> a -> LocTm -> Network m ()
send sid a l = Perf (Send sid a l)

recv :: Binary a => SessionId -> LocTm -> Network m a
recv sid l = Perf (Recv sid l)

broadcast :: Binary a => SessionId -> a -> Network m ()
broadcast sid a = Perf (BCast sid a)

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
-- class Backend c where
--   runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a
