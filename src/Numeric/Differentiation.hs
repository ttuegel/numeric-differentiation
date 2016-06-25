{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Numeric.Differentiation ( central ) where

import Numeric.Errors

import Scalar


-- | Compute the derivative using the 5-point rule, evaluating the function
-- at @x - h@, @x - h / 2@, @x + h / 2@, and @x + h@. (The central point is not
-- used.)
--
-- Compute the error using the difference between the 5-point rule and the
-- 3-point rule, evaluated at @x - h@, @x@, and @x + h@. (Again, the central
-- point is not used. This actually overestimates the error.)
--
-- The function to be differentiated is taken not as one acting on scalar values,
-- but as a function acting on a collection of values; this allows the caller to
-- enforce any compatibility conditions between function evaluations.
central :: ( Floating x, Ord x, Scalar x r, Error r ~ x
           , Num r, RoundingError r, TruncationError r ) =>
           (forall t. Traversable t => t x -> t r)
           -- ^ the function to differentiate
        -> x  -- ^ evaluate the derivative at @x@
        -> x  -- ^ initial step size
        -> (r, Error r)  -- ^ result and error
central f x h0 =
    let
        scale_ s = scale (asTypeOf s x)
        central_ h =
            let
                [fm1, fmh, fph, fp1] = f [x - h, x - h / 2, x + h / 2, x + h]

                -- result using 3-point rule
                result3 = scale_ 0.5 (fp1 - fm1)

                -- result using 5-point rule
                result5 = scale_ (4.0 / 3.0) (fph - fmh)
                          - scale_ (1.0 / 3.0) result3

                -- rounding error in 3-point rule
                error3 = roundingError fp1 + roundingError fm1

                -- rounding error in 5-point rule
                error5 = 2.0 * (roundingError fph + roundingError fmh) + error3

                -- rounding error due to finite precision in x + h = O(eps * x)
                errorPrec =
                    let
                        errorPrec3 = roundingError result3 / abs h
                        errorPrec5 = roundingError result5 / abs h
                    in
                      max errorPrec3 errorPrec5 * abs (x / h)

                result = scale (recip h) result5

                -- The truncation error in the result5 approximation is O(h^4).
                -- For safety, we estimate the error from result5 - result3,
                -- which is O(h^2). Scaling h minimizes this estimated error,
                -- not the actual truncation error.
                errorTrunc = truncationError result5 result3 / abs h

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
             && (errOpt < err0 && truncationError rOpt r0 < 4.0 * err0)
      then (rOpt, errOpt)
      else (r0, err0)


-- | Compute the derivative using the 4-point rule, evaluating the function
-- at @x + h / 4@, @x + h / 2@, @x + 3 h / 4@, and @x + h@.
--
-- Compute the error using the difference between the 4-point rule and the
-- 2-point rule, evaluated at @x + h / 2@, and @x + h@. (This actually
-- overestimates the error.)
--
-- The function to be differentiated is taken not as one acting on scalar values,
-- but as a function acting on a collection of values; this allows the caller to
-- enforce any compatibility conditions between function evaluations.
forward :: ( Floating x, Ord x, Scalar x r, Error r ~ x
           , Num r, RoundingError r, TruncationError r ) =>
           (forall t. Traversable t => t x -> t r)
           -- ^ the function to differentiate
        -> x  -- ^ evaluate the derivative at @x@
        -> x  -- ^ initial step size
        -> (r, Error r)  -- ^ result and error
forward f x h0 =
    let
        scale_ s = scale (asTypeOf s x)
        forward_ h =
            let
                fs@[f1, f2, f3, f4] =
                    f [x + h / 4, x + h / 2, x + 3 * h / 4, x + h]

                -- result using 2-point rule
                result2 = scale_ 2 (f4 - f2)

                -- result using 4-point rule
                result4 = scale_ (22 / 3) (f4 - f3)
                          - scale_ (62 / 3) (f3 - f2)
                          + scale_ (52 / 3) (f2 - f1)

                -- rounding error from the 4-point rule
                error4 = 2 * 20.67 * sum (roundingError <$> fs)

                -- rounding error due to finite precision in x + h = O(eps * x)
                errorPrec =
                    let
                        errorPrec2 = roundingError result2 / abs h
                        errorPrec4 = roundingError result4 / abs h
                    in
                      max errorPrec2 errorPrec4 * abs (x / h)

                result = scale (recip h) result4

                -- The truncation error in the @result4@ approximation itself is
                -- O(h^3). For safety, we estimate the error from
                -- @truncationError result4 result2@, which is O(h). By
                -- scaling @h@ we will minimize this estimated error, not
                -- the actual error in @result4@.
                errorTrunc = truncationError result4 result2 / abs h

                -- Rounding error
                errorRound = error4 / abs h + errorPrec
            in
              (result, errorTrunc, errorRound)

        (r0, trunc0, round0) = forward_ h0
        err0 = round0 + trunc0

        -- Compute an optimized step size to minimize the total error,
        -- using the scaling of the truncation error O(h) and rounding
        -- error O(1/h).
        hOpt = h0 * (round0 / trunc0) ** (1.0 / 2.0)
        (rOpt, truncOpt, roundOpt) = forward_ hOpt
        errOpt = roundOpt + truncOpt
    in
      if (round0 < trunc0 && round0 > 0 && trunc0 > 0)
             && (errOpt < err0 && truncationError rOpt r0 < 4.0 * err0)
      then (rOpt, errOpt)
      else (r0, err0)

backward :: ( Floating x, Ord x, Scalar x r, Error r ~ x
            , Num r, RoundingError r, TruncationError r ) =>
            (forall t. Traversable t => t x -> t r)
                -- ^ the function to differentiate
         -> x  -- ^ evaluate the derivative at @x@
         -> x  -- ^ initial step size
         -> (r, Error r)  -- ^ result and error
backward f x h0 = forward f x (negate h0)
