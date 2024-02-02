{-# LANGUAGE FunctionalDependencies #-}
module Screen where

import Control.Monad.State (MonadState)
import Data.Word (Word8)
import Data.ByteString (ByteString)

class (MonadState s m) => Screen m s | s -> m where
    initialize :: m ()
    clear :: m ()
    draw :: [Word8] -> Word8 -> Word8 -> m Bool
    serialize :: Word8 -> Word8 -> m ByteString
    drawn :: m Bool