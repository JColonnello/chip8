{-# LANGUAGE TemplateHaskell #-}
module Emulator where

import qualified Memory
import Implementations.MVectorMemory
import Inputs
import Zoom
import CPU
import qualified Register as Regs
import Register (GeneralRegister)
import Implementations.DictRegisters

import Control.Lens (makeLenses, use)
import qualified Data.ByteString as BS
import Control.Monad.ST
import Control.Monad.State.Strict as State
import Debug.Trace (traceM, traceShowM)
import Data.Bits.Extras (w8)
import Data.Word (Word16, Word8)


data EmulatorData s = EmulatorData {
    _mem :: MemState s,
    _screen :: BS.ByteString,
    _inputs :: Inputs,
    _registers :: RegisterState
}
makeLenses ''EmulatorData

type Emulator s a = StateT (EmulatorData s) (ST s) a

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

emulatorStep :: Emulator s ()
emulatorStep = do
    -- inputs ^>>= traceShowM
    fetchInstruction >>= executeInstruction . decodeInstruction
    -- screen %= BS.map (+1)

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

fetchInstruction :: Emulator s Word16
fetchInstruction = do
    pc <- registers <@> Regs.getPtrReg Regs.ProgramCounter
    registers <@> Regs.setPtrReg Regs.ProgramCounter (pc+2)
    mem <@> Memory.get16 pc

decodeInstruction :: Word16 -> Opcode
decodeInstruction = decode


executeInstruction :: Opcode -> Emulator s ()
executeInstruction op = case op of
    Jump (OpNNN nnn)                    -> jump nnn
    SetRegister (OpReg reg) (OpNN nn)   -> setRegister reg nn
    AddRegister (OpReg reg) (OpNN nn)   -> addRegister reg nn
    SetIndex (OpNNN nnn)                -> setIndex nnn
    _                                   -> return ()
    
jump :: Word16 -> Emulator s ()
jump nnn = registers <@> Regs.setPtrReg Regs.ProgramCounter nnn

setIndex :: Word16 -> Emulator s ()
setIndex nnn = registers <@> Regs.setPtrReg Regs.IndexRegister nnn

setRegister :: GeneralRegister -> Word8 -> Emulator s ()
setRegister reg nn = registers <@> Regs.setVarReg reg nn

addRegister :: GeneralRegister -> Word8 -> Emulator s ()
addRegister reg nn = do
    v <- registers <@> Regs.getVarReg reg
    registers <@> Regs.setVarReg reg (v+nn)