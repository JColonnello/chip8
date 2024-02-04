{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Implementations.MVectorMemory(MemState) where

import Data.Vector.Unboxed.Mutable (STVector, new, PrimMonad (PrimState), MVector, IOVector, drop, length, generate, copy, readMaybe, write, slice)
import Data.Word (Word8)
import Memory
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString as ByteString
import Control.Lens
import Data.Primitive
import Control.Monad
import Prelude hiding (length, drop)
import GHC.Data.Maybe
import Control.Monad.Trans.Class

newtype MemState' m = MemState' {
    vector :: MVector (PrimState m) Word8
}
type MemState s = MemState' (ST s)

-- g :: PrimMonad m => m (MemState' m)
-- g = go where
--     -- go :: forall s1. ST s1 (MemState' (MVector (PrimState (ST s1)) Word8))
--     go =  do
--         v <- new 4096 
--         return $ MemState' v

type MVMem s = (StateT (MemState' (ST s)) (ST s))
instance Memory (MVMem s) (MemState' (ST s)) where
    empty = do
        v <- new 4096
        State.put $ MemState' v
    load off_w bs = do
        m <- State.get
        let off = fromIntegral off_w
        arr <- ((\l -> generate (ByteString.length l) (ByteString.index l)) . ByteString.take (4096-off)) bs
        copy (slice off (length arr) . vector $ m) arr
    get off = do
        let r = flip readMaybe . fromIntegral $ off
        v <- State.get >>= (r . vector)
        return $ v `orElse` 0
    set off v = do
        let r m = write m (fromIntegral off) v
        State.get >>= (r . vector)