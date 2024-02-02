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

import Control.Lens (makeLenses, use, (%%=), (.=), (%=), uncons)
import qualified Data.ByteString as BS
import Control.Monad.ST
import Control.Monad.State.Strict as State
import Debug.Trace (traceM, traceShowM, traceShowId)
import Data.Bits.Extras (w8, w16)
import Data.Word (Word16, Word8, Word64)
import Data.Bool (bool)
import System.Random ( StdGen, RandomGen (genWord8) )
import Data.Bits
import GHC.Data.Maybe (orElse)
import Data.Function (on)
import Control.Applicative (liftA2)

data EmulatorData s = EmulatorData {
    _mem :: MemState s,
    _screen :: ScreenState s,
    _inputs :: Inputs,
    _currentTime :: Word64,
    _registers :: RegisterState,
    _randomGen :: StdGen,
    _stack :: [Word16]
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

lift2Reg :: (Word8 -> Word8 -> a) -> (GeneralRegister -> GeneralRegister -> Emulator s a)
lift2Reg f = on (liftA2 f) (zoom registers . Regs.getVarReg)

getReg :: GeneralRegister -> Emulator s Word8
getReg x = registers <@> Regs.getVarReg x

executeInstruction :: Opcode -> Emulator s ()
executeInstruction op = case traceShowId op of
    ClearScreen                         -> clearScreen
    Display (OpReg x) (OpReg y) (OpN n) -> display x y n
    Return                              -> return'
    Instruction2Regs op (OpReg x) (OpReg y) -> case op of
        BitOr                           -> bitOp x y (.|.)
        BitAnd                          -> bitOp x y (.&.)
        BitXor                          -> bitOp x y xor
        SkipEq                          -> skipCmp x y True
        SkipNEq                         -> skipCmp x y False
        SetRegister                     -> setRegister x y
        AddRegister                     -> addRegister x y
        SubstractXY                     -> substract x y False
        SubstractYX                     -> substract x y True
        ShiftRight                      -> shiftRegR x
        ShiftLeft                       -> shiftRegL x
    InstructionNNN op (OpNNN nnn) -> case op of
        Jump                            -> jump nnn
        SetIndex                        -> setIndex nnn
        Call                            -> call nnn
        CallNative                      -> error "Opcode not implemented"
        JumpOffset                      -> jumpOffset nnn
    InstructionRegNN op (OpReg reg) (OpNN nn) -> case op of
        SetRegisterN                    -> setRegisterN reg nn
        AddRegisterN                    -> addRegisterN reg nn
        Random                          -> random reg nn
        SkipEqN                         -> skipCmpN reg nn True
        SkipNEqN                        -> skipCmpN reg nn False
    InstructionReg op (OpReg reg)  -> case op of
        SkipPressed                     -> skipKey reg True
        SkipNotPressed                  -> skipKey reg False
        AddIndex                        -> addIndex reg
        GetKey                          -> getKey reg
        FontChar                        -> fontChar reg
        DecimalConv                     -> decimalConv reg
        StoreRegisters                  -> storeRegisters reg
        LoadRegisters                   -> loadRegisters reg
    ErrorCode                           -> error "Opcode not implemented"

storeRegisters :: GeneralRegister -> Emulator s ()
storeRegisters reg = do
    let list = [0..Regs.regGtoN reg]
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    let save off = getReg (Regs.nToRegG off) >>= (\v -> mem <@> Memory.set (vi + w16 off) v)
    mapM_ save list

loadRegisters :: GeneralRegister -> Emulator s ()
loadRegisters reg = do
    let list = [0..Regs.regGtoN reg]
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    let load off = (mem <@> Memory.get (vi + w16 off)) >>= setRegisterN (Regs.nToRegG off)
    mapM_ load list

decimalConv :: GeneralRegister -> Emulator s ()
decimalConv reg = do
    v <- getReg reg
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    let (c1,r1) = v `divMod` 10
    let (c2,r2) = c1 `divMod` 10
    mem <@> Memory.set vi r1
    mem <@> Memory.set (vi+1) r2
    mem <@> Memory.set (vi+2) c2

fontChar :: GeneralRegister -> Emulator s ()
fontChar = getReg >=> setIndex . (+fontLocation) . w16 . (*5) . (0xF .&.)

getKey :: GeneralRegister -> Emulator s ()
getKey reg = do
    key <- inputs <^&> firstPressed
    pc <- registers <@> Regs.getPtrReg Regs.ProgramCounter
    case key of
        Nothing -> jump (pc-2)
        Just k  -> setRegisterN reg . inputToN $ k

addIndex :: GeneralRegister -> Emulator s ()
addIndex reg = do
    v <- getReg reg
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    let nv = vi + w16 v
    setFlag (vi < 0x1000 && nv >= 0x1000)
    setIndex nv

skipKey :: GeneralRegister -> Bool -> Emulator s ()
skipKey reg pressed = do
    let test = fmap (== pressed) <$> (inputs <^&> isPressed)
    let input = nToInput <$> getReg reg
    skip <- test <*> input
    when skip $ jumpOffset 2

setFlag :: Bool -> Emulator s ()
setFlag = setRegisterN Regs.flagRegister . bool 0 1

carryOp :: (Word8 -> Word8 -> Word8) -> (Word8 -> Word8 -> Bool) -> GeneralRegister -> GeneralRegister -> Emulator s ()
carryOp f g x y = do
    let joinCarry _x _y = (f _x _y, g _x _y)
    (join, carry) <- lift2Reg joinCarry x y
    setRegisterN x join
    setFlag carry

addRegister :: GeneralRegister -> GeneralRegister -> Emulator s ()
addRegister = carryOp (+) ((>=0x80) .: (.&.))

substract :: GeneralRegister -> GeneralRegister -> Bool -> Emulator s ()
substract x y flipped = carryOp (flip' (-)) (flip' (>=)) x y
    where flip' = bool id flip flipped

setRegister :: GeneralRegister -> GeneralRegister -> Emulator s ()
setRegister x y = getReg y >>= setRegisterN x

shiftReg :: Int -> Int -> GeneralRegister -> Emulator s ()
shiftReg b off reg = do
    v <- getReg reg
    setFlag $ testBit v b
    setRegisterN reg (v `shift` off)

shiftRegR :: GeneralRegister -> Emulator s ()
shiftRegR = shiftReg 0 (-1)

shiftRegL :: GeneralRegister -> Emulator s ()
shiftRegL = shiftReg 7 1

skipCmp :: GeneralRegister -> GeneralRegister -> Bool -> Emulator s ()
skipCmp x y eq = lift2Reg ((== eq) .: (==)) x y >>= flip when (jumpOffset 2)

bitOp :: GeneralRegister -> GeneralRegister -> (Word8 -> Word8 -> Word8) -> Emulator s ()
bitOp x y op = lift2Reg op x y >>= setRegisterN x

skipCmpN :: GeneralRegister -> Word8 -> Bool -> Emulator s ()
skipCmpN reg nn eq = do
    v <- getReg reg
    when ((v == nn) == eq) $ jumpOffset 2

jumpOffset :: Word16 -> Emulator s ()
jumpOffset off = do
    pc <- registers <@> Regs.getPtrReg Regs.ProgramCounter
    jump $ pc + off

jump :: Word16 -> Emulator s ()
jump nnn = registers <@> Regs.setPtrReg Regs.ProgramCounter nnn

setIndex :: Word16 -> Emulator s ()
setIndex nnn = registers <@> Regs.setPtrReg Regs.IndexRegister nnn

setRegisterN :: GeneralRegister -> Word8 -> Emulator s ()
setRegisterN reg nn = registers <@> Regs.setVarReg reg nn

addRegisterN :: GeneralRegister -> Word8 -> Emulator s ()
addRegisterN reg nn = do
    v <- getReg reg
    setRegisterN reg (v+nn)

clearScreen :: Emulator s ()
clearScreen = screen <@> Screen.clear

display :: GeneralRegister -> GeneralRegister -> Word8 -> Emulator s ()
display _x _y _n = do
    let n = fromIntegral _n
    x <- getReg _x
    y <- getReg _y
    vi <- registers <@> Regs.getPtrReg Regs.IndexRegister
    sprite <- mem <@> mapM Memory.get [vi..vi+n-1]
    flipped <- screen <@> Screen.draw sprite x y
    setFlag flipped

random :: GeneralRegister -> Word8 -> Emulator s ()
random reg mask = do
    rnum <- randomGen %%= genWord8
    registers <@> Regs.setVarReg reg (mask .&. rnum)

call :: Word16 -> Emulator s ()
call addr = do
    pc <- registers <@> Regs.getPtrReg Regs.ProgramCounter
    stack %= (pc:)
    jump addr

return' :: Emulator s ()
return' = do
    addr <- stack %%= ((`orElse` (0,[]))  . uncons)
    jump addr