{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Main where

import Control.Monad.ST
import Control.Monad.State.Strict as State
import qualified Data.Word as Word
import qualified Data.ByteString as BS
import Foreign.C.Types
import Control.Lens (makeLenses, (^.), set, (%~))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Debug.Trace (traceM, traceShowM)
import Data.Bits.Extras (w8)
import Control.Concurrent (threadDelay)

import Zoom
import Implementations.MVectorMemory
import qualified Memory

import SDL.Vect
import qualified SDL
import SDL (Event, Keycode, EventPayload (KeyboardEvent), InputMotion, Renderer, Texture)
import SDL.Input
import SDL.Event

data EmulatorData s = EmulatorData { _mem :: MemState s, _screen :: BS.ByteString }
makeLenses ''EmulatorData

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

type Emulator s a = StateT (EmulatorData s) (ST s) a

runEmulator :: EmulatorData RealWorld -> Emulator RealWorld a -> IO (a, EmulatorData RealWorld)
runEmulator state emulator = stToIO $ runStateT emulator state

emulatorStep :: Emulator s BS.ByteString
emulatorStep = do
    -- zoom mem $ Memory.set 0 1
    -- zoom mem $ Memory.set 1 3
    State.modify (screen %~ BS.map (+1))
    -- zoom mem $ sum <$> sequence (Memory.get <$> [1])
    State.gets (^.screen)

texWidth, texHeight :: CInt
(texWidth, texHeight) = (64, 32)
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (texWidth*10, texHeight*10) 

appLoop :: Renderer -> Texture -> EmulatorData RealWorld -> IO ()
appLoop renderer texture state = loop Set.empty state where
    loop oldKeys oldState = do
        
        events <- SDL.pollEvents
        let keys = pressedKeys oldKeys events

        -- Step emulator
        (rawScreen, state) <- runEmulator oldState emulatorStep
        let screen = rawScreen;

        SDL.updateTexture texture Nothing screen texWidth
        SDL.copy renderer texture Nothing Nothing
        SDL.present renderer

        let exit = hasExit events || elem KeycodeEscape keys
        threadDelay (1000000 `div` 90)
        unless exit (loop keys state)

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
    texture <- SDL.createTexture renderer SDL.RGB332 SDL.TextureAccessStreaming (V2 texWidth texHeight)

    bs <- BS.readFile "test.bin"
    initialState <- snd <$> runEmulator (EmulatorData {}) ( do
        zoom mem Memory.empty
        zoom mem $ Memory.load 0 bs
        -- BS.replicate (64*32) 111
        let bitmap = w8 . (`mod` 256) <$> [1..texWidth*texHeight]
        State.modify (set screen $ BS.pack bitmap)
        )

    appLoop renderer texture initialState

    SDL.destroyRenderer renderer
    SDL.destroyWindow window