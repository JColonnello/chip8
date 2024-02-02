{-# LANGUAGE FunctionalDependencies #-}
module Timer where

import Control.Monad.State (MonadState)
import Data.Word (Word8, Word64)

class (MonadState s m) => Timer m s | s -> m where
    initialize :: m ()
    getDelayTimer :: m Word8
    getSoundTimer :: m Word8
    setNSTime :: Word64 -> m ()
    setDelayTimer :: Word8 -> m ()
    setSoundTimer :: Word8 -> m ()