{-# LANGUAGE TemplateHaskell #-}
module Emulator where

import qualified Memory
import Implementations.MVectorMemory
import Inputs
import Zoom
import CPU
import qualified Register as Regs
import Register (GeneralRegister)
import qualified Screen
import Implementations.DictRegisters
import Implementations.WordMVScreen (ScreenState)

import Control.Lens (makeLenses, use, (%%=), (.=))
import qualified Data.ByteString as BS
import Control.Monad.ST
import Control.Monad.State.Strict as State
import Debug.Trace (traceM, traceShowM, traceShowId)
import Data.Bits.Extras (w8)
import Data.Word (Word16, Word8, Word64)
import Data.Bool (bool)
import System.Random ( StdGen, RandomGen (genWord8) )
import Data.Bits

data EmulatorData s = EmulatorData {
    _mem :: MemState s,
    _screen :: ScreenState s,
    _inputs :: Inputs,
    _currentTime :: Word64,
    _registers :: RegisterState,
    _randomGen :: StdGen
}
makeLenses ''EmulatorData

type Emulator s a = StateT (EmulatorData s) (ST s) a

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

fontLocation :: Word16
fontLocation = 0x50

emulatorStep :: Emulator s ()
emulatorStep = do
    fetchInstruction >>= executeInstruction . decodeInstruction

loadEmulator :: BS.ByteString -> BS.ByteString -> StdGen -> Emulator s ()
loadEmulator bs font gen = do
    mem <@> Memory.empty
    mem <@> Memory.load fontLocation font
    mem <@> Memory.load 0x200 bs
    screen <@> Screen.initialize
    registers <@> Regs.initialize
    registers <@> Regs.setPtrReg Regs.ProgramCounter 0x200
    randomGen .= gen

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
executeInstruction op = case traceShowId op of
    ClearScreen                         -> clearScreen
    Display (OpReg x) (OpReg y) (OpN n) -> display x y n
    Instruction2Regs op (OpReg x) (OpReg y) -> case op of
        _ -> error "Opcode not implemented"
    InstructionNNN op (OpNNN nnn) -> case op of
        Jump                            -> jump nnn
        SetIndex                        -> setIndex nnn
    InstructionRegNN op (OpReg reg) (OpNN nn) -> case op of
        SetRegisterN                    -> setRegisterN reg nn
        AddRegisterN                    -> addRegisterN reg nn
        Random                          -> random reg nn
    InstructionReg op (OpReg reg)  -> case op of
        _ -> error "Opcode not implemented"
    ErrorCode                           -> error "Opcode not implemented"

jump :: Word16 -> Emulator s ()
jump nnn = registers <@> Regs.setPtrReg Regs.ProgramCounter nnn

setIndex :: Word16 -> Emulator s ()
setIndex nnn = registers <@> Regs.setPtrReg Regs.IndexRegister nnn

setRegisterN :: GeneralRegister -> Word8 -> Emulator s ()
setRegisterN reg nn = registers <@> Regs.setVarReg reg nn

addRegisterN :: GeneralRegister -> Word8 -> Emulator s ()
addRegisterN reg nn = do
    v <- registers <@> Regs.getVarReg reg
    registers <@> Regs.setVarReg reg (v+nn)

clearScreen :: Emulator s ()
clearScreen = screen <@> Screen.clear

display :: GeneralRegister -> GeneralRegister -> Word8 -> Emulator s ()
display _x _y _n = do
    let n = fromIntegral _n
    x <- registers <@> Regs.getVarReg _x
    y <- registers <@> Regs.getVarReg _y
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    sprite <- mem <@> mapM Memory.get [vi..vi+n-1]
    flipped <- screen <@> Screen.draw sprite x y
    registers <@> Regs.setVarReg Regs.flagRegister (bool 0 1 flipped)

random :: GeneralRegister -> Word8 -> Emulator s ()
random reg mask = do
    rnum <- randomGen %%= genWord8
    registers <@> Regs.setVarReg reg (mask .&. rnum)