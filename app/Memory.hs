module Memory where

import Data.ByteString
import Data.Word

class MemoryState ms where
    init :: ms

class Memory mem where
    empty :: mem ()
    load :: Word16 -> ByteString -> mem ()
    get :: Word16 -> mem Word8
    set :: Word16 -> Word8 -> mem ()