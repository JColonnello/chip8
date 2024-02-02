{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Implementations.SimpleTimer where

import Timer
import Data.Word (Word64)
import Control.Monad.Trans.State.Strict as State
import Data.Bits.Extras (w8, w64)

data TimerState = TimerState {
    currentTime :: Word64,
    delayTarget :: Word64,
    soundTarget :: Word64
}

remaining timer s =
    let time = currentTime s
        target = timer s
        r = if target > time then target - time else 0
    in w8 $ (r * 60) `div` 1000000000

type SimpleTimer = State TimerState
instance Timer SimpleTimer TimerState where
  initialize = State.put $ TimerState 0 0 0
  getDelayTimer = State.gets $ remaining delayTarget
  getSoundTimer = State.gets $ remaining soundTarget
  setNSTime t = State.modify $ \s -> s { currentTime = t}
  setDelayTimer v = State.modify $ \s -> s { delayTarget = w64 v * (1000000000 `div` 60) + currentTime s}
  setSoundTimer v = State.modify $ \s -> s { soundTarget = w64 v * (1000000000 `div` 60) + currentTime s}
