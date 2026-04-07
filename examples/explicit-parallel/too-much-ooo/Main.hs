{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Choreography
-- import Choreography.Choreo
import Data.Proxy
import GHC.TypeLits
import System.Environment

$(mkLoc "alice")
$(mkLoc "bob")

data FileReading a where
  Open :: FileReading ()
  Read :: FileReading String
  Close :: FileReading ()

choreo :: Choreo FileReading (String @ "alice")
choreo = do
  alice `locally` (\un -> Open)
  s <- alice `locally` (\un -> Read)
  alice `locally` (\un -> Close)
  return s

-- choreo_epp :: Choreography.Choreo.Epp FileReading (String @ "alice")
-- choreo_epp = Choreography.Choreo.epp choreo "alice"

main :: IO ()
main = pure ()

