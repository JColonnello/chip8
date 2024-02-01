{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Implementations.WordMVScreen where

import Data.Vector.Unboxed.Mutable as V (MVector, PrimMonad (PrimState), set, new, foldl, read, modify, write, foldr)
import Data.Word (Word8, Word64)
import Control.Monad.ST.Strict (ST)
import Control.Monad.State.Strict as State
import Screen
import Data.Function ((&))
import Data.ByteString (ByteString, pack)
import Data.Bool (bool)
import Data.Bits (testBit, (.&.), shift, xor, popCount)
import Data.Bits.Extras (w64)
import Control.Lens (imapM)
import Debug.Trace (traceShowId)

newtype ScreenData' m = ScreenData' {
    vector :: MVector (PrimState m) Word64
}
type ScreenState s = ScreenData' (ST s)

type MVScreen s = (StateT (ScreenData' (ST s)) (ST s))
instance Screen (MVScreen s) (ScreenData' (ST s)) where
    initialize = new 32 >>= State.put . ScreenData'
    clear = State.get >>= flip set 0 . vector
    serialize c0 c1 =
        let c = bool c0 c1
            foldW w = map (c . testBit w) [63,62..0]
            foldV = V.foldr ((++) . foldW) []
        in  pack <$> (State.get >>= foldV . vector)
    draw _sprite _x _y =
        let x = fromIntegral _x .&. 63
            y = fromIntegral _y .&. 31
            sprite = take (32 - y) _sprite
            shifted sr = shift (w64 sr) (64 - 8 - x)
            drawRow v i sr = do
                row <- V.read v (y+i)
                let shiftedS = shifted sr
                V.write v (y+i) (row `xor` shiftedS)
                return $ popCount (row .&. shiftedS) > 0
        in State.get >>= (\v -> or <$> imapM (drawRow v) sprite) . vector