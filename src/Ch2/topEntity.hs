module Ch2.Project (topEntity) where

-- This is the Clash equivalent of the Haskell Prelude
import Clash.Prelude
import Clash.Annotations.TH

-- What `main` is for a Haskell program, `topEntity` is for Clash.
topEntity
  :: "BTN" ::: Signal System Bit
  -> "LED" ::: Signal System Bit
topEntity = id

makeTopEntity 'topEntity
