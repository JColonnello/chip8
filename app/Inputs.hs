module Inputs where
import Data.Set as Set
import Control.Exception (assert)
import Data.Bits.Extras (w8)
import Data.Word (Word8)

newtype InputKey = InputKey { number :: Word8 } deriving (Eq, Ord, Show)
newtype Inputs = Inputs { keys :: Set InputKey } deriving (Show)

nToInput :: Integral a => a -> InputKey
-- nToInput n  = assert(n >= 0 || n <= 15) . InputKey . w8 $ n
nToInput = InputKey . w8

inputToN :: InputKey -> Word8
inputToN = number

isPressed :: Inputs -> InputKey -> Bool
isPressed = flip elem . keys

firstPressed :: Inputs -> Maybe InputKey
firstPressed = lookupMin . keys