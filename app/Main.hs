{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.ST
import Control.Monad.State.Strict as State
import qualified Data.Word as Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C8
import Prelude as P
import Foreign.C.Types
import Control.Lens (makeLenses)
import Data.Set (Set)
import qualified Data.Set as Set

import Zoom

import Implementations.MVectorMemory
import Memory

import SDL.Vect
import qualified SDL
import SDL
import Data.Maybe (mapMaybe)

newtype EmulatorData s = EmulatorData { _mem :: MemState s }
makeLenses ''EmulatorData

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

type Emulator a = forall s. StateT (EmulatorData s) (ST s) a

runEmulator :: forall a. Emulator a -> IO a
runEmulator emulator = stToIO go
    where   go :: forall i. ST i a
            go = evalStateT emulator undefEmulatorData

emulatorMain :: Emulator Word.Word8
emulatorMain = do
    -- zoom mem $ Memory.set 0 1
    -- zoom mem $ Memory.set 1 3
    -- zoom mem $ sum <$> sequence (Memory.get <$> [1])
    zoom mem $ Memory.get 0

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

appLoop :: SDL.Renderer -> SDL.Texture -> Set Keycode -> IO ()
appLoop renderer texture oldKeys = do

    -- bs <- BS.readFile "test.bin"
    -- BS.C8.putStrLn bs
    -- i <- runEmulator $ do
    --     zoom mem Memory.empty
    --     zoom mem $ Memory.load 0 bs
    --     emulatorMain
    -- P.putStrLn "End"
    -- print i
    events <- pollEvents
    let keys = pressedKeys oldKeys events

    -- TODO Step emulator
    let screen = BS.pack $ P.replicate (fromIntegral $ screenHeight*screenWidth) 111

    SDL.updateTexture texture Nothing screen screenWidth
    SDL.copy renderer texture Nothing Nothing
    present renderer

    let exit = hasExit events || elem KeycodeEscape keys
    unless exit (appLoop renderer texture keys)

hasExit :: [Event] -> Bool
hasExit = any (isExit . SDL.eventPayload) where
    isExit SDL.QuitEvent = True
    isExit _ = False

pressedKeys :: Set Keycode -> [Event] -> Set Keycode
pressedKeys oldKeys events = oldKeys `Set.difference` released `Set.union` pressed where
    pressed = filterEvent Pressed
    released = filterEvent Released
    filterEvent e = Set.fromList $ fst <$> filter ((== e) . snd) keyEvents
    keyEvents = mapMaybe (keyEvent . eventPayload) events
    keyEvent :: EventPayload -> Maybe (Keycode, InputMotion)
    keyEvent (KeyboardEvent ke) = Just (keysymKeycode . keyboardEventKeysym $ ke, keyboardEventKeyMotion ke)
    keyEvent _ = Nothing

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "CHIP-8 Emulator" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGB332 SDL.TextureAccessStreaming (V2 64 32)

    appLoop renderer texture Set.empty

    SDL.destroyRenderer renderer
    SDL.destroyWindow window