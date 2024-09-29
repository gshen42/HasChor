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
    Located s m a ->
    ChoreoSig ls m (Async a @ r)
  Cond ::
    forall s {b} {ls} {m} {a}.
    (Typeable s, Member s ls, TypeableList ls, Show a, Read a) =>
    Located s m a ->
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
  Located s m a ->
  Choreo ls m (Async a @ r)
comm act = perform (Comm @s @r act)

commSync ::
  forall s r {ls} {m} {a}.
  (Typeable s, Typeable r, Member s ls, Member r ls, Show a, Read a, MonadIO m) =>
  Located s m a ->
  Choreo ls m (a @ r)
commSync act = do
  fut <- comm @s @r act
  locally $ do
    a <- unwrap fut
    liftIO $ wait a

cond ::
  forall s {b} {ls} {m} {a}.
  (Typeable s, Member s ls, TypeableList ls, Show a, Read a) =>
  Located s m a ->
  (a -> Choreo ls m b) ->
  Choreo ls m b
cond act k = perform (Cond @s act k)

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

data EppState = EppState
  { seqIdMap :: HashMap LocTm SeqId,
    sentMsgs :: [Async ()]
  }

initState :: EppState
initState =
  EppState
    { seqIdMap = HM.empty,
      sentMsgs = []
    }

-- Get the next sequence ID for a location (return 0 if there's none) and set
-- the next sequence ID to the current value plus 1.
newSeqId :: forall l {m}. (Typeable l, MonadState EppState m) => m SeqId
newSeqId = do
  s <- get
  let l = reify @l
  let v = HM.findWithDefault 0 l (seqIdMap s)
  put $ s {seqIdMap = HM.insert l (v + 1) (seqIdMap s)}
  return v

waitSentMsgs :: (MonadIO m) => [Async ()] -> m ()
waitSentMsgs l = forM_ l (liftIO . wait)

epp ::
  forall t {ls} {m} {a}.
  (Typeable t, MonadIO m) =>
  Choreo ls m a ->
  Network m a
epp c = do
  (a, s) <- runStateT (epp1 @t c) initState
  waitSentMsgs (sentMsgs s)
  return a

epp1 ::
  forall t {ls} {m} {a} {mm}.
  (Typeable t, MonadIO m) =>
  Choreo ls m a ->
  StateT EppState (Network m) a
epp1 = interp (handler @t)

-- TODO: better way to write this?
lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 = lift . lift

handler ::
  forall t {ls} {m} {a} {mm}.
  (Typeable t, MonadIO m) =>
  ChoreoSig ls m a ->
  StateT EppState (Network m) a
handler (Locally @l act)
  | eqLoc @t @l = do
      a <- lift2 $ unLocated act
      return (Wrap a)
  | otherwise = return Empty
handler (Comm @s @r act)
  | eqLoc @s @r = do
      a <- lift2 $ unLocated act
      aa <- liftIO $ async (return a)
      return (Wrap aa)
  | eqLoc @t @s = do
      id <- newSeqId @s
      a <- lift2 $ unLocated act
      lift $ send a (reify @r) id
      return Empty
  | eqLoc @t @r = do
      id <- newSeqId @s
      aa <- lift $ recv (reify @s) id
      return (Wrap aa)
  | otherwise = return Empty
handler (Cond @s act k)
  | eqLoc @t @s = do
      id <- newSeqId @s
      a <- lift2 $ unLocated act
      mapM_ (\r -> lift $ send a r id) (reifyList @ls)
      epp1 @t (k a)
  | otherwise = do
      id <- newSeqId @s
      aa <- lift $ recv (reify @s) id
      a <- liftIO $ wait aa
      epp1 @t (k a)
handler (LocallyFork @l act) = undefined
handler (CommFork @s @r act) = undefined
