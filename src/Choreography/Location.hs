-- | This module defines locations and functions for working with them.
module Choreography.Location where

import GHC.TypeLits
import Language.Haskell.TH hiding (Loc)

-- | A type-level location paired with a term-level proxy of it.
data Loc (l :: Symbol) where
  Loc :: forall proxy l. (KnownSymbol l) => proxy l -> Loc l

-- | Equality of type-level locations.
eqLoc :: Loc l -> Loc l' -> Bool
eqLoc (Loc p) (Loc p') = symbolVal p == symbolVal p'

-- | A term-level locaiton.
type LocTm = String

-- | Convert a type-level location to a term-level location.
toLocTm :: Loc l -> LocTm
toLocTm (Loc p) = symbolVal p

-- | Define a location at both type and term levels.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let p = mkName "Data.Proxy.Proxy"
  pure [SigD locName (AppT (ConT p) (LitT (StrTyLit loc))), ValD (VarP locName) (NormalB (ConE p)) []]
