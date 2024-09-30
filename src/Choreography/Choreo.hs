{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Choreography.Choreo where

import Choreography.Located
import Choreography.Location
import Choreography.Network
import Control.Concurrent.Async (Async, async, wait)
import Control.Monad (forM_)
import Control.Monad.Freer
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.Typeable (Typeable)

-- | Signature of the `Choreo` monad.
data ChoreoSig ls m a where
  Locally ::
    forall l {ls} {m} {a}.
    (Typeable l, Member l ls) =>
    Located l m a ->
    ChoreoSig ls m (a @ l)
  Comm ::
    forall s r {ls} {m} {a}.
    (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
    a @ s ->
    ChoreoSig ls m (Async a @ r)
  Cond ::
    forall s {b} {ls} {m} {a}.
    (Typeable s, Member s ls, TypeableList ls, Show a, Read a) =>
    a @ s ->
    (a -> Choreo ls m b) ->
    ChoreoSig ls m b
  LocallyFork ::
    forall l {ls} {m} {a}.
    (Typeable l, Member l ls) =>
    Located l m a ->
    ChoreoSig ls m (Async a @ l)
  CommFork ::
    forall s r {ls} {m} {a}.
    (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
    Located s m a ->
    ChoreoSig ls m (Async a @ r)

-- | The monad for choreographies.
type Choreo ls m a = Freer (ChoreoSig ls m) a

-- TODO: is there a way to avoid repeating the type signatures?

locally ::
  forall l {ls} {m} {a}.
  (Typeable l, Member l ls) =>
  Located l m a ->
  Choreo ls m (a @ l)
locally act = perform (Locally act)

-- TODO: use the `async-lifted` package to make the following two functions
-- work for any monad m that implements MonadIO

comm ::
  forall s r {ls} {m} {a}.
  (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
  a @ s ->
  Choreo ls m (Async a @ r)
comm val = perform (Comm @s @r val)

commSync ::
  forall s r {ls} {m} {a}.
  (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a, MonadIO m) =>
  a @ s ->
  Choreo ls m (a @ r)
commSync val = do
  fut <- comm @s @r val
  locally $ do
    a <- unwrap fut
    liftIO $ wait a

cond ::
  forall s {b} {ls} {m} {a}.
  (Typeable s, Member s ls, TypeableList ls, Show a, Read a) =>
  a @ s ->
  (a -> Choreo ls m b) ->
  Choreo ls m b
cond val k = perform (Cond @s val k)

locallyFork ::
  forall l {ls} {m} {a}.
  (Typeable l, Member l ls) =>
  Located l m a ->
  Choreo ls m (Async a @ l)
locallyFork act = perform (LocallyFork act)

commFork ::
  forall s r {ls} {m} {a}.
  (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a) =>
  Located s m a ->
  Choreo ls m (Async a @ r)
commFork act = perform (CommFork @s @r act)

type ChoreoIO ls a = Choreo ls IO a

type SeqNumMap = HashMap (LocTm, LocTm) SeqNum

-- Get the next sequence number for a location pair (return 0 if there's none)
-- and set the next sequence number to the current value plus 1.
newSeqNum :: (MonadState SeqNumMap m) => (LocTm, LocTm) -> m SeqNum
newSeqNum key = do
  map <- get
  let v = HM.findWithDefault 0 key map
  put (HM.insert key (v + 1) map)
  return v

epp ::
  forall t {ls} {m} {a}.
  (Typeable t, MonadIO m) =>
  Choreo ls m a ->
  Network m a
epp c = do
  (a, _) <- runStateT (epp1 @t c) HM.empty
  return a

epp1 ::
  forall t {ls} {m} {a} {mm}.
  (Typeable t, MonadIO m) =>
  Choreo ls m a ->
  StateT SeqNumMap (Network m) a
epp1 = interp (handler @t)

-- TODO: better way to write this?
lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 = lift . lift

handler ::
  forall t {ls} {m} {a} {mm}.
  (Typeable t, MonadIO m) =>
  ChoreoSig ls m a ->
  StateT SeqNumMap (Network m) a
handler (Locally @l act)
  | eqLoc @t @l = do
      a <- lift2 $ unLocated act
      return (Wrap a)
  | otherwise = return Empty
handler (Comm @s @r val)
  | eqLoc @s @r = do
      let a = unsafeUnwrap val
      aa <- liftIO $ async (return a)
      return (Wrap aa)
  | eqLoc @t @s = do
      let a = unsafeUnwrap val
      id <- newSeqNum (reify @s, reify @r)
      lift $ send a (reify @r) id
      return Empty
  | eqLoc @t @r = do
      id <- newSeqNum (reify @s, reify @r)
      aa <- lift $ recv (reify @s) id
      return (Wrap aa)
  | otherwise = return Empty
handler (Cond @s val k)
  | eqLoc @t @s = do
      let a = unsafeUnwrap val
      mapM_
        ( \r -> do
            id <- newSeqNum (reify @s, r)
            lift $ send a r id
        )
        (reifyList @ls)
      epp1 @t (k a)
  | otherwise = do
      id <- newSeqNum (reify @s, reify @t)
      aa <- lift $ recv (reify @s) id
      a <- liftIO $ wait aa
      epp1 @t (k a)
handler (LocallyFork @l act) = undefined
handler (CommFork @s @r act) = undefined
