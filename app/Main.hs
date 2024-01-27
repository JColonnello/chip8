{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.Trans.State as State
import Control.Lens
    ( makeLenses, LensLike', Zoomed )
import Data.Word
import Data.ByteString as BS
import Data.ByteString.Char8 as BS.C8
import Prelude as P

import Implementations.DictMemory
import Memory
import Control.Monad.ST
import qualified Control.Lens.Internal.Zoom as LZ
import Control.Monad.State.Class (MonadState)
import Control.Monad.Identity
import qualified Control.Lens.Zoom as Zoom

newtype EmulatorData = EmulatorData { _mem :: MemState }
makeLenses ''EmulatorData

clearEmulatorData :: EmulatorData
clearEmulatorData = EmulatorData Memory.init

type Emulator a = forall s. StateT EmulatorData (ST s) a

resetEmulator :: Emulator ()
resetEmulator = put clearEmulatorData

runEmulator :: forall a. Emulator a -> IO a
runEmulator emulator = return $ runST go
    where   go :: forall i. ST i a
            go = evalStateT emulator clearEmulatorData

emulatorMain :: StateT EmulatorData (ST s) Word8
emulatorMain = do
    -- zoom mem $ Memory.set 0 1
    -- zoom mem $ Memory.set 1 3
    -- zoom mem $ sum <$> sequence (Memory.get <$> [1])
    zoom mem (State.get :: State MemState MemState)
    zoom mem $ Memory.get 0
class (MonadState s m, MonadState t n) => Zoom m n s t | m -> s, n -> t where
    zoom :: LensLike' (Zoomed n c) t s -> m c -> n c

instance Monad z => Zoom (StateT s Identity) (StateT t z) s t where 
    zoom l (StateT m) = StateT $ LZ.unfocusing #. l (LZ.Focusing #. (trans . m)) where
        trans :: Identity x -> z x
        trans = return . runIdentity

instance Monad z => Zoom (StateT s z) (StateT t z) s t where 
    zoom = Zoom.zoom

main :: IO ()
main = do
    bs <- BS.readFile "test.bin"
    BS.C8.putStrLn bs
    i <- runEmulator $ do 
        zoom mem $ Memory.load 0 bs
        emulatorMain
    P.putStrLn "End"
    print i
    