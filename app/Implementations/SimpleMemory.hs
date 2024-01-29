{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Implementations.SimpleMemory (
    MemState
) where

import Control.Monad.Trans.State.Strict as State
import Memory
import Data.Word
import GHC.Data.Maybe
import Control.Lens
import Data.ByteString as ByteString
import Data.Bits.Extras

type MemState = [(Word16, Word8)]
clearMemState :: MemState
clearMemState = []

instance MemoryState MemState where
    -- init = clearMemState

type SimpleMem = State MemState

instance Memory SimpleMem MemState where
    empty = put clearMemState
    load off bs = do
        m <- State.get
        let zip = (itoList . unpack) $ ByteString.take (fromIntegral $ 4096-off) bs
        let w = zip <&> _1 %~ ((+off).w16)
        let nm = Prelude.foldl (flip $ uncurry recordarRep) m w
        put nm
    get off = do
        m <- State.get
        let value = lookup off m
        return $ orElse value 0
    set off v = modify (recordarRep off v)

recordarRep v n [] = [(v,n)]
recordarRep v n ((v',n'):vns) =
    if v==v'
    then (v',n) : vns
    else (v',n') : recordarRep v n vns