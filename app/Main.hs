{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Trans.State
import Control.Lens
import GHC.Data.Maybe
import Control.Monad

type MemState = [(String, Int)]
clearMemState :: MemState
clearMemState = []

type Mem a = State MemState a

runMem :: Mem a -> a
runMem m = evalState m []

clear :: Mem ()
clear = put clearMemState

getVar :: String -> Mem Int
getVar v = do
    m <- get
    let value = lookup v m
    return $ orElse value 0

setVar :: String -> Int -> Mem ()
setVar v i = modify (recordarRep v i)



recordarRep v n [] = [(v,n)]
recordarRep v n ((v',n'):vns) =
    if v==v'
    then (v',n) : vns
    else (v',n') : recordarRep v n vns

newtype EmulatorData = EmulatorData { _mem :: MemState }
makeLenses ''EmulatorData

clearEmulatorData :: EmulatorData
clearEmulatorData = EmulatorData clearMemState

type Emulator a = State EmulatorData a

resetEmulator :: Emulator ()
resetEmulator = put clearEmulatorData

runEmulator :: Emulator a -> IO a
runEmulator emulator = return $ evalState emulator clearEmulatorData

emulatorMain :: Emulator Int
emulatorMain = do
    zoom mem $ setVar "a" 1
    zoom mem $ setVar "b" 3
    zoom mem $ sum <$> sequence (getVar <$> ["a","b"])

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    i <- runEmulator emulatorMain
    print i