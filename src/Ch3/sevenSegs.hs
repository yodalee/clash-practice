module Ch3 (sevenSegs) where

-- This is the Clash equivalent of the Haskell Prelude
import Clash.Prelude
import Clash.Annotations.TH
import qualified Data.List as L
import RetroClash.Utils

ledStrip
  :: "BTNS" ::: Signal System (Vec 2 Bit)
  -> "LEDS" ::: Signal System (Vec 4 Bit)
ledStrip switches = fmap (map boolToBit) . bundle $
  both :> either :> onlyOne :> onlyTheSecond :> Nil
  where
    sw1 :> sw2 :> Nil = unbundle $ map bitToBool <$> switches
    both = sw1 .&&. sw2
    either = sw1 .||. sw2
    onlyOne = sw1 ./=. sw2
    onlyTheSecond = (not <$> sw1) .&&. sw2

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines . L.concat $
  [ L.replicate 1 $ horiz  a
   ,L.replicate 3 $ vert  f b
   ,L.replicate 1 $ horiz  g
   ,L.replicate 3 $ vert  e c
   ,L.replicate 1 $ horiz  d
  ]
  where
    horiz True  = " ###### "
    horiz False = " ...... "
    vert b1 b2 = part b1 <> "      " <> part b2
      where
        part True = "#"
        part False = "."

ss5 :: Vec 7 Bool
ss5 = True :> True :> False :> True :> False :> True :> True :> Nil

encodeHexSS :: Unsigned 4 -> Vec 7 Bool
encodeHexSS n = unpack $ case n of
  --       abcdefg
  0x0 -> 0b1111110
  0x1 -> 0b0110000
  0x2 -> 0b1101101
  0x3 -> 0b1111001
  0x4 -> 0b0110011
  0x5 -> 0b1011011
  0x6 -> 0b1011111
  0x7 -> 0b1110010
  0x8 -> 0b1111111
  0x9 -> 0b1111011
  0xa -> 0b1110111
  0xb -> 0b0011111
  0xc -> 0b1001110
  0xd -> 0b0111101
  0xe -> 0b1001111
  0xf -> 0b1000111

demux :: Unsigned 1 -> Vec 2 Bool
demux n = unpack $ case n of
  0x0 -> 0b01
  0x1 -> 0b10


-- Notes about the map to lpf file
-- It will be extracted to: BTNS[3] :> BTNS[2] :> BTNS[1] :> BTNS[0] :> Nil
-- Same to LEDS
sevenSegs
  :: (
    "SEL" ::: Signal System Bit,
    "BTNS" ::: Signal System (Vec 4 Bit)
  )
  -> (
    "ANODES" ::: Signal System (Vec 2 (Active High)),
    "LEDS" ::: Signal System (Vec 7 (Active Low))
  )
sevenSegs (sel, btns) =
  (map toActive <$> anodes,
   map toActive <$> segments)
  where
    selbool = fmap bitToBool sel
    anodes = bundle $ selbool :> (fmap not selbool) :> Nil
    digits = bitCoerce <$> btns
    segments = encodeHexSS <$> digits

makeTopEntity 'sevenSegs

