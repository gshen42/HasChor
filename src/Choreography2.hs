{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- | An one-off module for testing Choreo2.hs
module Choreography2 (
  -- * Locations and Located Values
  -- LocTm,
  -- LocTy,
  -- type (@),
  mkLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  cond,
  par,

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
  mkHttpConfig,

  -- * Running choreographies
  -- runChoreo,
  runChoreography,
  ) where

-- import Choreography.Location
import Choreography.Choreo2
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.Tree
import Control.Monad.Reader
import GHC.TypeLits.Singletons
import Language.Haskell.TH

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, KnownSymbol l') =>
  config -> Choreo IO l' a -> SSymbol l' -> IO a
runChoreography cfg c l' = runNetwork cfg (toLocTm l') (runReaderT (unEpp (epp c l')) Root)

-- | Define a location at both type and term levels.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let p = mkName "GHC.TypeLits.SSymbol"
  pure [SigD locName (AppT (ConT p) (LitT (StrTyLit loc))),ValD (VarP locName) (NormalB (ConE p)) []]
