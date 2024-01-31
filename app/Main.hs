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
import Control.Lens (makeLenses, use)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Debug.Trace (traceM, traceShowM)
import Data.Bits.Extras (w8)
import Control.Concurrent (threadDelay)
import Data.List (singleton)

import Inputs
import Zoom
import Implementations.MVectorMemory
import qualified Memory

import SDL.Vect
import qualified SDL
import SDL (Event, Keycode, EventPayload (KeyboardEvent), InputMotion, Renderer, Texture)
import SDL.Input
import SDL.Event

data EmulatorData s = EmulatorData { _mem :: MemState s, _screen :: BS.ByteString, _inputs :: Inputs }
makeLenses ''EmulatorData

undefEmulatorData :: EmulatorData s
undefEmulatorData = undefEmulatorData

type Emulator s a = StateT (EmulatorData s) (ST s) a

runEmulator :: EmulatorData RealWorld -> Emulator RealWorld a -> IO (a, EmulatorData RealWorld)
runEmulator state emulator = stToIO $ runStateT emulator state

emulatorStep :: Emulator s ()
emulatorStep = do
    inputs ^>>= traceShowM
    screen %= BS.map (+1)

texWidth, texHeight :: CInt
(texWidth, texHeight) = (64, 32)
screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (texWidth*10, texHeight*10)

mapMaybeSet :: (Ord a2) => (a1 -> Maybe a2) -> Set a1 -> Set a2
mapMaybeSet f = Set.fromList . Set.foldl (\b -> maybe b (:b) . f) [] 

appLoop :: Renderer -> Texture -> EmulatorData RealWorld -> IO ()
appLoop renderer texture state = loop Set.empty state where
    loop oldKeys oldState = do
        events <- SDL.pollEvents
        let keys = pressedKeys oldKeys events
        let inputKeys = Inputs (mapMaybeSet keyToInput keys)

        let step = do
                inputs .= inputKeys
                emulatorStep
                use screen

        -- Step emulator
        (rawScreen, state) <- runEmulator oldState step
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

keyToInput :: Keycode -> Maybe InputKey
keyToInput key = nToInput <$> case key of 
    Keycode1 -> Just 0x1
    Keycode2 -> Just 0x2
    Keycode3 -> Just 0x3
    Keycode4 -> Just 0xC
    KeycodeQ -> Just 0x4
    KeycodeW -> Just 0x5
    KeycodeE -> Just 0x6
    KeycodeR -> Just 0xD
    KeycodeA -> Just 0x7
    KeycodeS -> Just 0x8
    KeycodeD -> Just 0x9
    KeycodeF -> Just 0xE
    KeycodeZ -> Just 0xA
    KeycodeX -> Just 0x0
    KeycodeC -> Just 0xB
    KeycodeV -> Just 0xF
    _ -> Nothing

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "CHIP-8 Emulator" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.createTexture renderer SDL.RGB332 SDL.TextureAccessStreaming (V2 texWidth texHeight)

    bs <- BS.readFile "test.bin"
    initialState <- snd <$> runEmulator (EmulatorData {}) ( do
        mem <%=> Memory.empty
        mem <%=> Memory.load 0 bs
        -- BS.replicate (64*32) 111
        let bitmap = w8 . (`mod` 256) <$> [1..texWidth*texHeight]
        screen .= BS.pack bitmap
        )

    appLoop renderer texture initialState

    SDL.destroyRenderer renderer
    SDL.destroyWindow window