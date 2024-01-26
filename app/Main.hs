{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Trans.State as State
import Control.Lens
import Memory
import Data.Word
import Data.ByteString as BS
import Data.ByteString.Char8 as BS.C8
import Prelude as P

import Implementations.DictMemory

newtype EmulatorData = EmulatorData { _mem :: MemState }
makeLenses ''EmulatorData

clearEmulatorData :: EmulatorData
clearEmulatorData = EmulatorData Memory.init

type Emulator a = State EmulatorData a

resetEmulator :: Emulator ()
resetEmulator = put clearEmulatorData

runEmulator :: Emulator a -> IO a
runEmulator emulator = return $ evalState emulator clearEmulatorData

-- emulatorMain :: Emulator String
emulatorMain = do
    -- zoom mem $ Memory.set 0 1
    -- zoom mem $ Memory.set 1 3
    -- zoom mem $ sum <$> sequence (Memory.get <$> [1])
    zoom mem State.get
    -- zoom mem $ Memory.get 0

main :: IO ()
main = do
    bs <- BS.readFile "test.bin"
    BS.C8.putStrLn bs
    i <- runEmulator $ do 
        zoom mem $ Memory.load 0 bs
        emulatorMain
    P.putStrLn "End"
    print i
    