{-# LANGUAGE FlexibleInstances #-}
module Implementations.DictMemory (
    MemState
) where

import qualified Data.IntMap.Strict as Map
import Data.Word
import Memory
import Control.Monad.Trans.State as State
import Data.ByteString as ByteString
import Control.Lens
import Data.Bits.Extras
import GHC.Data.Maybe
import Debug.Trace
import Control.Monad.IO.Class (MonadIO(liftIO))

type MemState = Map.IntMap Word8
instance MemoryState MemState where
    init = Map.empty

type DictMem = State MemState
instance Memory DictMem where
    empty = put Memory.init
    load off_w bs = do
        m <- State.get
        let off = fromIntegral off_w
        let zip = (itoList . unpack) $ ByteString.take (4096-off) bs
        let w = zip <&> _1 %~ (+off)
        let nm = Map.fromList w
        let newState = Map.union nm m
        put newState
    get = let m = (Map.lookup . fromIntegral)
        in (((`orElse` 0) <$>) . State.gets) . m
    set off v = State.modify $ Map.insert (fromIntegral off) v