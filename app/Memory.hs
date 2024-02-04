{-# LANGUAGE FunctionalDependencies #-}
module Memory where

import Data.ByteString
import Data.Word
import Control.Monad.State.Class
import Data.Bits (Bits(..))
import Control.Monad (liftM2)
import Data.Bits.Extras (w16)
import Data.Function (on)

class (MonadState s m) => Memory m s | s -> m where
    empty :: m ()
    load :: Word16 -> ByteString -> m ()
    get :: Word16 -> m Word8
    set :: Word16 -> Word8 -> m ()
    get16 :: (Memory m s) => Word16 -> m Word16
    get16 i = on (liftM2 fuse) Memory.get i (i+1) where
        fuse :: Word8 -> Word8 -> Word16
        fuse a b = shiftL (w16 a) 8 .|. w16 b