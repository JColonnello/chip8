module CPU (
    decode, 
    Opcode(..), 
    OpNNN(..), 
    OpNN(..), 
    OpN(..), 
    OpReg(..),
    Instruction2Regs(..), 
    InstructionNNN(..), 
    InstructionRegNN(..), 
    InstructionReg(..), 
) where
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), Bits (shiftR, (.|.), shiftL))
import Data.Bits.Extras (w8, w16)
import Register (GeneralRegister (GeneralRegister))

type Nibble = Word8
data Instruction = Instruction Nibble Nibble Nibble Nibble

divide :: Word16 -> Instruction
divide x = Instruction 
    (w8 $ shiftR (x .&. 0xF000) 12)
    (w8 $ shiftR (x .&. 0xF00) 8)
    (w8 $ shiftR (x .&. 0xF0) 4)
    (w8 $ x .&. 0xF)

pack2 :: Nibble -> Nibble -> Word8
pack2 a b = shiftL a 4 .|. b 

pack3 :: Nibble -> Nibble -> Nibble -> Word16
pack3 a b c = shiftL (w16 a) 8 .|. shiftL (w16 b) 4 .|. w16 c

newtype OpNNN = OpNNN Word16 deriving (Show)
newtype OpNN = OpNN Word8 deriving (Show)
newtype OpN = OpN Nibble deriving (Show)
newtype OpReg = OpReg GeneralRegister deriving (Show)

data Instruction2Regs = 
        BitOr 
    |   BitAnd
    |   BitXor
    |   SkipEq
    |   SkipNEq
    |   SetRegister
    |   AddRegister
    |   SubstractXY
    |   SubstractYX
    |   ShiftRight
    |   ShiftLeft
    deriving (Show)

data InstructionNNN =
        Jump
    |   SetIndex
    |   Call
    |   CallNative
    |   JumpOffset
    deriving (Show)

data InstructionRegNN =
        SetRegisterN
    |   AddRegisterN
    |   SkipEqN
    |   SkipNEqN
    |   Random
    deriving (Show)

data InstructionReg =
        SkipPressed
    |   SkipNotPressed
    |   GetTimer
    |   SetDelayTimer
    |   SetSoundTimer
    |   AddIndex
    |   GetKey
    |   FontChar
    |   DecimalConv
    |   StoreRegisters
    |   LoadRegisters
    deriving (Show)

data Opcode = 
        ClearScreen
    |   Return
    |   Display OpReg OpReg OpN
    |   Instruction2Regs Instruction2Regs OpReg OpReg
    |   InstructionNNN InstructionNNN OpNNN
    |   InstructionRegNN InstructionRegNN OpReg OpNN
    |   InstructionReg InstructionReg OpReg
    |   ErrorCode
    deriving (Show)

decode :: Word16 -> Opcode
decode = parse . divide where
    parse (Instruction a b c d) = 
        let nnn = OpNNN $ pack3 b c d
            nn = OpNN $ pack2 c d
            x = OpReg $ GeneralRegister b
            y = OpReg $ GeneralRegister c
            n = OpN d

            instruction2Regs op = Instruction2Regs op x y 
            instructionNNN op = InstructionNNN op nnn 
            instructionRegNN op = InstructionRegNN op x nn
            instructionReg op = InstructionReg op x
        in case (a,b,c,d) of 

        (0,0,0xE,0)     -> ClearScreen
        (0,0,0xE,0xE)   -> Return
        (0xD,_,_,_)     -> Display x y n
        (0,_,_,_)       -> instructionNNN CallNative
        (1,_,_,_)       -> instructionNNN Jump
        (2,_,_,_)       -> instructionNNN Call
        (0xA,_,_,_)     -> instructionNNN SetIndex
        (0xB,_,_,_)     -> instructionNNN JumpOffset
        (3,_,_,_)       -> instructionRegNN SkipEqN
        (4,_,_,_)       -> instructionRegNN SkipNEqN
        (6,_,_,_)       -> instructionRegNN SetRegisterN
        (7,_,_,_)       -> instructionRegNN AddRegisterN
        (0xC,_,_,_)     -> instructionRegNN Random
        (5,_,_,0)       -> instruction2Regs SkipEq
        (8,_,_,0)       -> instruction2Regs SetRegister
        (8,_,_,1)       -> instruction2Regs BitOr
        (8,_,_,2)       -> instruction2Regs BitAnd
        (8,_,_,3)       -> instruction2Regs BitXor
        (8,_,_,4)       -> instruction2Regs AddRegister
        (8,_,_,5)       -> instruction2Regs SubstractXY
        (8,_,_,6)       -> instruction2Regs ShiftRight
        (8,_,_,7)       -> instruction2Regs SubstractYX
        (8,_,_,0xE)     -> instruction2Regs ShiftLeft
        (9,_,_,0)       -> instruction2Regs SkipNEq
        (0xE,_,9,0xE)   -> instructionReg SkipPressed
        (0xE,_,0xA,1)   -> instructionReg SkipNotPressed
        (0xF,_,0,7)     -> instructionReg GetTimer
        (0xF,_,0,0xA)   -> instructionReg GetKey
        (0xF,_,1,5)     -> instructionReg SetDelayTimer
        (0xF,_,1,8)     -> instructionReg SetSoundTimer
        (0xF,_,1,0xE)   -> instructionReg AddIndex
        (0xF,_,2,9)     -> instructionReg FontChar
        (0xF,_,3,3)     -> instructionReg DecimalConv
        (0xF,_,5,5)     -> instructionReg StoreRegisters
        (0xF,_,6,5)     -> instructionReg LoadRegisters
        _ -> ErrorCode
