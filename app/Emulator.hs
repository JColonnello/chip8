{-# LANGUAGE TemplateHaskell #-}
module Emulator where

import qualified Memory
import Implementations.MVectorMemory
import Inputs
import Zoom

import Control.Lens (makeLenses, use)
import qualified Data.ByteString as BS
import Control.Monad.ST
import Control.Monad.State.Strict as State
import Debug.Trace (traceM, traceShowM)
import Data.Bits.Extras (w8)


data EmulatorData s = EmulatorData { _mem :: MemState s, _screen :: BS.ByteString, _inputs :: Inputs }
makeLenses ''EmulatorData

type Emulator s a = StateT (EmulatorData s) (ST s) a

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

emulatorStep :: Emulator s ()
emulatorStep = do
    -- inputs ^>>= traceShowM
    screen %= BS.map (+1)

loadEmulator :: BS.ByteString -> Emulator s ()
loadEmulator bs = do
    mem <@> Memory.empty
    mem <@> Memory.load 0 bs
    mem <@> Memory.set 0 1
    mem <@> Memory.set 1 3
    mem <@> Memory.get16 0 >>= traceShowM
    -- BS.replicate (64*32) 111
    let bitmap = w8 . (`mod` 256) <$> [1..64*32]
    screen .= BS.pack bitmap

runEmulator :: EmulatorData RealWorld -> Emulator RealWorld a -> IO (a, EmulatorData RealWorld)
runEmulator state emulator = stToIO $ runStateT emulator state