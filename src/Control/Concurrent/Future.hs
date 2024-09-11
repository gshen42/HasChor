-- | A very basic futures library built on top of `MVar`s.
module Control.Concurrent.Future where

import Control.Concurrent.MVar    

newtype Future a = Future {unFuture :: MVar a}
