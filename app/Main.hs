{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Lens (makeLenses)
import Data.Word
import Data.ByteString as BS
import Data.ByteString.Char8 as BS.C8
import Prelude as P
import Zoom
import Implementations.MVectorMemory
import Memory
import Control.Monad.ST
import Control.Monad.State.Strict as State

newtype EmulatorData s = EmulatorData { _mem :: MemState s }
makeLenses ''EmulatorData

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

type Emulator a = forall s. StateT (EmulatorData s) (ST s) a

runEmulator :: forall a. Emulator a -> IO a
runEmulator emulator = stToIO go
    where   go :: forall i. ST i a
            go = evalStateT emulator undefEmulatorData

emulatorMain :: Emulator Word8
emulatorMain = do
    -- zoom mem $ Memory.set 0 1
    -- zoom mem $ Memory.set 1 3
    -- zoom mem $ sum <$> sequence (Memory.get <$> [1])
    zoom mem $ Memory.get 0

-- render :: SDL.Renderer -> Emulator SDL.Texture
-- render renderer = do
--     surface <- create

main :: IO ()
main = do
    bs <- BS.readFile "test.bin"
    BS.C8.putStrLn bs
    i <- runEmulator $ do
        zoom mem Memory.empty
        zoom mem $ Memory.load 0 bs
        emulatorMain
    P.putStrLn "End"
    print i
    