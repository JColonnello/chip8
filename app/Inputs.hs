module Inputs where
import Data.Set as Set
import Control.Exception (assert)

newtype InputKey = InputKey { number :: Int } deriving (Eq, Ord, Show)
newtype Inputs = Inputs { keys :: Set InputKey } deriving (Show)

nToInput :: Int -> InputKey
nToInput n  = assert(n >= 0 || n <= 15) $ InputKey n

inputToN :: InputKey -> Int
inputToN = number

isPressed :: InputKey -> Inputs -> Bool
isPressed key = elem key . keys
