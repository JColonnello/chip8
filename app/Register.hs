{-# LANGUAGE FunctionalDependencies #-}
module Register(
    PointerRegister(..),
    GeneralRegister(..),
    Registers(..),
) where
import Data.Word
import Control.Monad.State (MonadState)
import Control.Exception (assert)
import Data.Bits.Extras (w8)

data PointerRegister = ProgramCounter | IndexRegister deriving (Eq, Ord)
newtype GeneralRegister = GeneralRegister { regGtoN :: Word8 } deriving (Eq, Ord)

ntoRegG n = assert (n >= 0 && n <= 0xF) $ GeneralRegister $ w8 n
flagRegister = GeneralRegister 0xF

class (MonadState s m) => Registers m s | s -> m where
    setPtrReg :: PointerRegister -> Word16 -> m ()
    getPtrReg :: PointerRegister -> m Word16
    getVarReg :: GeneralRegister -> m Word8
    setVarReg :: GeneralRegister -> Word8 -> m ()