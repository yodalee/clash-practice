module Ch4 (
  helloRegister,
  flipRegister,
  blink) where

-- This is the Clash equivalent of the Haskell Prelude
import Clash.Explicit.Prelude
import Clash.Annotations.TH
import qualified Data.List as L

helloRegister :: Clock System -> Reset System -> Enable System -> Signal System Bool
helloRegister clk rst en = register clk rst en True (pure False)

flipRegister :: Clock System -> Reset System -> Enable System -> Signal System Bool
flipRegister clk rst en = r
  where r = register clk rst en True (not <$> r)

flipTwice :: Clock System -> Reset System -> Enable System -> Signal System Bool
flipTwice clk rst en = r2
  where r1 = register clk rst en True r2
        r2 = register clk rst en True (not <$> r1)

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom

blink :: forall dom. (KnownDomain dom)
  => (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom))
  => (1 <= 1_000_000_000_000 `Div` DomainPeriod dom)
  => Clock dom -> Reset dom -> Enable dom -> Signal dom Bit
blink clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (SecondPeriods dom)))
    r = register clk rst en 0 (r + 1)


data OnOff on off =
    On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x
  | x == maxBound = Nothing
  | otherwise = Just $ succ x
predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x
  | x == minBound = Nothing
  | otherwise = Just $ pred x

isOn :: OnOff on off -> Bool
isOn On {}  = True
isOn Off {} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ succIdx y

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq
type ClockDivider dom ps = ps `Div` DomainPeriod dom

blinkSecond :: forall dom. (KnownDomain dom)
  => (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom))
  => (1 <= HzToPeriod 1 `Div` DomainPeriod dom)
  => Clock dom -> Reset dom -> Enable dom -> Signal dom Bit
blinkSecond clk rst en = boolToBit . isOn <$> r
  where
    r :: Signal dom (
      OnOff (ClockDivider dom (500_000_000_000))
            (ClockDivider dom (500_000_000_000)))
    r = register clk rst en (Off 0) $ countOnOff <$> r

createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_000_000}

blinkEntity
  :: "clk" ::: Clock Dom25
  -> "LED" ::: Signal Dom25 Bit
blinkEntity clk = blinkSecond clk resetGen enableGen

makeTopEntity 'blinkEntity
