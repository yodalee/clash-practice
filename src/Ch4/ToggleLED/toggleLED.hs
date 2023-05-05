module ToggleLED (toggleLED) where

-- This is the Clash equivalent of the Haskell Prelude
import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils

toggleLED ::
  "clk" ::: Clock System
  -> "btn" ::: Signal System (Active High)
  -> "led" ::: Signal System (Active High)
toggleLED = withResetEnableGen board
  where
    board btn = toActive <$> led
      where
        btn' = fromActive <$> btn
        click = btn' .&&. (not <$> register False btn')
        led = register False $ mux click (not <$> led) led

makeTopEntity 'toggleLED
