{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Main where

import qualified Data.Word as Word
import qualified Data.ByteString as BS
import Foreign.C.Types
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Bits.Extras (w8)
import Control.Concurrent (threadDelay)
import Data.List (singleton)
import qualified Screen

import Inputs
import Zoom
import Emulator

import SDL.Vect (V2(..))
import qualified SDL
import SDL (Event, Keycode, EventPayload (KeyboardEvent), InputMotion, Renderer, Texture)
import SDL.Input
import SDL.Event
import Control.Monad.ST (RealWorld, stToIO)
import Control.Lens (use, (.=))
import Control.Monad (unless, when)
import GHC.Clock (getMonotonicTimeNSec)
import System.Random (initStdGen)
import Timer (Timer(setNSTime, getSoundTimer))
import System.Environment (getArgs)
import Debug.Trace (traceShowId, traceShow, trace, traceM)
import Sound.Honk

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
        time <- getMonotonicTimeNSec

        let step = do
                inputs .= inputKeys
                timer <@> setNSTime time
                emulatorStep
                dirtyScreen <- screen <@> Screen.drawn
                screenBS <- if dirtyScreen then Just <$> (screen <@> Screen.serialize 0x0 0xFF) else return Nothing
                soundtimer <- timer <@> Timer.getSoundTimer
                return (screenBS, fromIntegral soundtimer)

        -- Step emulator
        ((rawScreen, soundTimer), state) <- runEmulator oldState step

        -- when (soundTimer > 0) $
        --         let duration = fromIntegral soundTimer / 60
        --         in playOne $ Note duration 110

        case rawScreen of
            Nothing -> return ()
            Just bs -> do
                -- traceM "Draw"
                SDL.updateTexture texture Nothing bs texWidth
                SDL.copy renderer texture Nothing Nothing
                SDL.present renderer

        let exit = hasExit events || elem KeycodeEscape keys
        threadDelay (1000000 `div` 1000)
        unless exit (loop keys state)

hasExit :: [Event] -> Bool
hasExit = any (isExit . SDL.eventPayload) where
    isExit SDL.QuitEvent = True
    isExit _ = False

pressedKeys :: Set Keycode -> [Event] -> Set Keycode
pressedKeys oldKeys events = (oldKeys `Set.union` pressed) `Set.difference` released where
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

    romFile <- head <$> getArgs
    bs <- BS.readFile romFile
    font <- BS.readFile "roms/font.bin"
    random <- initStdGen
    initialState <- snd <$> runEmulator (EmulatorData {}) (loadEmulator bs font random)

    SDL.present renderer
    appLoop renderer texture initialState

    SDL.destroyRenderer renderer
    SDL.destroyWindow window