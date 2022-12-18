{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds       #-}

module Advent.Linear.Fields
    ( _x
    , _y
    , _z
    ) where

import Data.Functor.Const (Const(..))
import Linear (V1, V2, V3, V4, _x, _y, _z, _w)
import GHC.Records (HasField(..))

view :: (forall f. Functor f => (a -> f b) -> s -> f t) -> s -> a
view l = getConst . l Const

-- probably won't need more than these for now

instance HasField "x" (V1 a) a where
    getField = view _x

instance HasField "x" (V2 a) a where
    getField = view _x

instance HasField "x" (V3 a) a where
    getField = view _x

instance HasField "x" (V4 a) a where
    getField = view _x

instance HasField "y" (V2 a) a where
    getField = view _y

instance HasField "y" (V3 a) a where
    getField = view _y

instance HasField "y" (V4 a) a where
    getField = view _y

instance HasField "z" (V3 a) a where
    getField = view _z

instance HasField "z" (V4 a) a where
    getField = view _z

instance HasField "w" (V4 a) a where
    getField = view _w
