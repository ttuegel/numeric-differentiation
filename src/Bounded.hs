{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Bounded where

import Data.Complex ( Complex(..) )
import Foreign.C.Types ( CDouble, CFloat )


type family Bound a


class Ord (Bound a) => BoundedAbove a where
    upperBound :: a -> Bound a


type instance Bound Float = Float

instance BoundedAbove Float where
    upperBound = abs


type instance Bound Double = Double

instance BoundedAbove Double where
    upperBound = abs


type instance Bound CFloat = CFloat

instance BoundedAbove CFloat where
    upperBound = abs


type instance Bound CDouble = CDouble

instance BoundedAbove CDouble where
    upperBound = abs


type instance Bound (Complex a) = Bound a

instance BoundedAbove a => BoundedAbove (Complex a) where
    upperBound (re :+ im) = max (upperBound re) (upperBound im)
