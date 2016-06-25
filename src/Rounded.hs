module Rounded where

import Foreign.C.Types ( CDouble, CFloat )
import Numeric.IEEE ( epsilon )

import Bounded


-- | Finite-precision types where a reasonable estimate of the upper bound
-- to the rounding error exists for any value.
class Rounded a where

    -- | An estimate of the rounding error of a value.
    rounded :: a -> Bound a


instance Rounded Float where
    rounded x = abs x * epsilon

instance Rounded Double where
    rounded x = abs x * epsilon

instance Rounded CFloat where
    rounded x = abs x * epsilon

instance Rounded CDouble where
    rounded x = abs x * epsilon
