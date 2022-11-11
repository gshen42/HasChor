--------------------------------------------------------------------------------
-- Monad for writing control programs with explicit sends and receives.
--------------------------------------------------------------------------------
module Choreography.Control (Control, send, recv, newEmptyMsgBufs, runControl) where

import Choreography.Location (Location)
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Control.Concurrent.Async (Async, async, wait)

-- Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap Location (Chan String)

type MsgBufs = HashMap Location MsgBuf

type Control = ReaderT (MsgBufs, Location) IO

send :: (Show a) => a -> Location -> Control ()
send x loc = do
    (bufs, self) <- ask
    let chan = (bufs ! loc) ! self
    lift $ writeChan chan (show x)

recv :: (Read a) => Location -> Control a
recv loc = do
    (bufs, self) <- ask
    let chan = (bufs ! self) ! loc
    lift $ fmap read (readChan chan)

-- TODO: Simplify and factor common pattern out of `newEmptyMsgBuf` and
-- `newEmptyMsgBufs`.
newEmptyMsgBuf :: [Location] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
    where
        f hash loc = do
            chan <- newChan
            return (HashMap.insert loc chan hash)

newEmptyMsgBufs :: [Location] -> IO MsgBufs
newEmptyMsgBufs locs = foldM f HashMap.empty locs
    where
        f hash loc = do
            buf <- newEmptyMsgBuf locs
            return (HashMap.insert loc buf hash)

-- TODO: allow control programs to return values of different types.
runControl :: [(Location, Control a)] -> IO [(Location, a)]
runControl l = do
    let locs = map fst l
    bufs    <- newEmptyMsgBufs locs
    asyncs  <- mapM (\(loc, prog) -> async (runReaderT prog (bufs, loc))) l
    results <- mapM wait asyncs
    return (zip locs results)
