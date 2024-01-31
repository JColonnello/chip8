module CPU (decode, Opcode(..), OpNNN(..), OpNN(..), OpN(..), OpReg(..)) where
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

newtype OpNNN = OpNNN Word16
newtype OpNN = OpNN Word8
newtype OpN = OpN Nibble
newtype OpReg = OpReg GeneralRegister
data Opcode = 
        ClearScreen
    |   Jump OpNNN
    |   SetRegister OpReg OpNN
    |   AddRegister OpReg OpNN
    |   SetIndex OpNNN
    |   Display OpReg OpReg OpN
    |   ErrorCode

decode :: Word16 -> Opcode
decode = parse . divide where
    parse (Instruction a b c d) = 
        let nnn = OpNNN $ pack3 b c d
            nn = OpNN $ pack2 c d
            x = OpReg $ GeneralRegister b
            y = OpReg $ GeneralRegister c
            n = OpN d
        in case (a,b,c,d) of 

        (0,0,0xE,0) -> ClearScreen
        (1,_,_,_) -> Jump nnn
        (6,_,_,_) -> SetRegister x nn
        (7,_,_,_) -> AddRegister x nn
        (0xA,_,_,_) -> SetIndex nnn
        (0xD,_,_,_) -> Display x y n
        _ -> ErrorCode
