{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Implementations.DictRegisters (RegisterState) where

import Register
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict as State (State, modify, put)
import Data.Word (Word16, Word8)
import Control.Lens
import Zoom ((.:), (<^&>))
import GHC.Data.Maybe (orElse)

data RegisterState = RegisterState {
    _ptrRegs :: Map PointerRegister Word16,
    _varRegs :: Map GeneralRegister Word8
}
makeLenses ''RegisterState

type DictRegisters = State RegisterState
instance Registers DictRegisters RegisterState where
    initialize = State.put $ RegisterState Map.empty Map.empty
    setPtrReg = (ptrRegs %=) .: Map.insert
    setVarReg = (varRegs %=) .: Map.insert
    getPtrReg r = (`orElse` 0) <$> ptrRegs <^&> Map.lookup r
    getVarReg r = (`orElse` 0) <$> varRegs <^&> Map.lookup r