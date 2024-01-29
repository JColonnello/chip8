{-# LANGUAGE FunctionalDependencies #-}
module Memory where

import Data.ByteString
import Data.Word
import Control.Monad.State.Class

class MemoryState s where
    -- init :: s

class (MemoryState s, MonadState s m) => Memory m s | s -> m where
    empty :: m ()
    load :: Word16 -> ByteString -> m ()
    get :: Word16 -> m Word8
    set :: Word16 -> Word8 -> m ()