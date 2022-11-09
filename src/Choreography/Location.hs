module Choreography.Location where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import GHC.Generics (Generic)

newtype Location = Location String
    deriving (Eq, Ord, Generic)

instance Hashable Location

instance IsString Location where
    fromString = Location
