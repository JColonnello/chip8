{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Zoom (Zoom.zoom) where

import Control.Monad.State.Class (MonadState)
import qualified Control.Lens.Zoom as Z
import qualified Control.Lens.Internal.Zoom as LZ
import  Control.Lens hiding (Zoom)
import Control.Monad.Identity
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

class (MonadState s m, MonadState t n) => Zoom m n s t | m -> s, n -> t where
    zoom :: LensLike' (Zoomed n c) t s -> m c -> n c

instance Monad z => Zoom (Lazy.StateT s Identity) (Lazy.StateT t z) s t where 
    zoom l (Lazy.StateT m) = Lazy.StateT $ LZ.unfocusing . l (LZ.Focusing . (trans . m)) where
        trans :: Identity x -> z x
        trans = return . runIdentity

instance Monad z => Zoom (Lazy.StateT s z) (Lazy.StateT t z) s t where 
    zoom = Z.zoom

instance Monad z => Zoom (Strict.StateT s Identity) (Strict.StateT t z) s t where 
    zoom l (Strict.StateT m) = Strict.StateT $ LZ.unfocusing . l (LZ.Focusing . (trans . m)) where
        trans :: Identity x -> z x
        trans = return . runIdentity

instance Monad z => Zoom (Strict.StateT s z) (Strict.StateT t z) s t where 
    zoom = Z.zoom