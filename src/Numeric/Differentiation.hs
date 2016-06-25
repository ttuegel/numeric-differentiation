{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Numeric.Differentiation ( central ) where

import Bounded
import Rounded
import Scalar


central :: ( BoundedAbove r, Bound r ~ x, Rounded r, BoundedAbove x
           , Floating x, Ord x, Num r, Rounded x, Scalar x r
           ) =>
           (forall t. Traversable t => t x -> t r) -> x -> x -> (r, x)
central f x h0 =
    let
        scale_ s = scale (asTypeOf s x)
        central_ h =
            let
                [fm1, fmh, fph, fp1] = f [x - h, x - h / 2, x + h / 2, x + h]

                -- result using 3-point rule
                result3 = scale_ 0.5 (fp1 - fm1)

                -- result using 5-point rule
                result5 =
                    scale_ (4.0 / 3.0) (fph - fmh) - scale_ (1.0 / 3.0) result3

                -- rounding error in 3-point rule
                error3 = roundingError fp1 + roundingError fm1

                -- rounding error in 5-point rule
                error5 = 2.0 * (roundingError fph + roundingError fmh) + error3

                roundingError3 = roundingError result3
                roundingError5 = roundingError result5
                -- rounding error due to finite precision in x + h = O(eps * x)
                errorPrec = max roundingError3 roundingError5 * abs (x / (h * h))

                result = scale (recip h) result5

                -- The truncation error in the result5 approximation is O(h^4).
                -- For safety, we estimate the error from result5 - result3,
                -- which is O(h^2). Scaling h minimizes this estimated error,
                -- not the actual truncation error.
                errorTrunc = upperBound (result5 - result3) / abs h

                -- Rounding error
                errorRound = error5 / abs h + errorPrec

            in
              (result, errorTrunc, errorRound)

        (r0, trunc0, round0) = central_ h0
        err0 = round0 + trunc0

        -- Compute an optimized step size to minimize the total error,
        -- using the scaling of the truncation error O(h^2) and rounding
        -- error O(1/h).
        hOpt = h0 * (round0 / (2.0 * trunc0)) ** (1.0 / 3.0)
        (rOpt, truncOpt, roundOpt) = central_ hOpt
        errOpt = roundOpt + truncOpt
    in
      if (round0 < trunc0 && round0 > 0 && trunc0 > 0)
             && (errOpt < err0 && upperBound (rOpt - r0) < 4.0 * err0)
      then (rOpt, errOpt)
      else (r0, err0)
