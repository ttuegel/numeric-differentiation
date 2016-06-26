{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Numeric.Differentiation ( central ) where

import Imprecise


data D a = D !a !a !a !a

instance Functor D where
    fmap f (D a b c d) = D (f a) (f b) (f c) (f d)
    {-# INLINE fmap #-}

instance Foldable D where
    foldr f z (D a b c d) = f a (f b (f c (f d z)))
    {-# INLINE foldr #-}

instance Traversable D where
    traverse f (D a b c d) = D <$> f a <*> f b <*> f c <*> f d
    {-# INLINE traverse #-}


-- | Compute the derivative using the 5-point rule, evaluating the function
-- at @x - h@, @x - h / 2@, @x + h / 2@, and @x + h@. (The central point is not
-- used.)
--
-- Compute the error using the difference between the 5-point rule and the
-- 3-point rule, evaluated at @x - h@, @x@, and @x + h@. (Again, the central
-- point is not used. This actually overestimates the error.)
--
-- The function to be differentiated is taken as a function acting on a
-- /collection/ of values; this allows the caller to enforce any compatibility
-- conditions between function evaluations.
central :: (Floating x, Num r, Imprecise r, Estimate r ~ x) =>
           (forall t. Traversable t => t x -> t r)
           -- ^ the function to differentiate
        -> x  -- ^ evaluate the derivative at @x@
        -> x  -- ^ initial step size
        -> (r, Estimate r)  -- ^ result and error
central f x h0 =
    let
        central_ h =
            let
                fs@(D fm1 fmh fph fp1) =
                    let
                        points = D (x - h) (x - h / 2) (x + h / 2) (x + h)
                    in
                      f points

                -- result using 3-point rule
                result3 = scale (1 / 2) (fp1 - fm1)

                -- result using 5-point rule
                result5 = scale (4 / 3) (fph - fmh)
                          - scale (1 / 3) result3

                result = scale (recip h) result5

                -- The truncation error in the result5 approximation is O(h^4).
                -- For safety, we estimate the error from result5 - result3,
                -- which is O(h^2). Scaling h minimizes this estimated error,
                -- not the actual truncation error.
                trunc = limiting (result5 - result3) / abs h

                -- Rounding error
                round =
                    let
                        -- rounding error in 5-point rule
                        rounding5 =
                            let
                                -- rounding error in each term of the result
                                D rm1 rmh rph rp1 = precision <$> fs
                            in
                              2 * (rph + rmh) + rp1 + rm1

                        -- rounding error due to finite precision in
                        -- x + h = O(eps * x)
                        prec =
                            let
                                prec3 = precision result3 / abs h
                                prec5 = precision result5 / abs h
                            in
                              max prec3 prec5 * abs (x / h)
                    in
                      rounding5 / abs h + prec

            in
              (result, trunc, round)

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
             && (errOpt < err0 && limiting (rOpt - r0) < 4.0 * err0)
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
forward :: ( Floating x, Ord x
           , Num r, Imprecise r, Estimate r ~ x ) =>
           (forall t. Traversable t => t x -> t r)
           -- ^ the function to differentiate
        -> x  -- ^ evaluate the derivative at @x@
        -> x  -- ^ initial step size
        -> (r, Estimate r)  -- ^ result and error
forward f x h0 =
    let
        scale_ s = scale (asTypeOf s x)
        forward_ h =
            let
                fs@(D f1 f2 f3 f4) =
                    let
                        points =
                            D (x + h / 4) (x + h / 2) (x + (3 / 4) * h) (x + h)
                    in
                      f points

                -- result using 2-point rule
                result2 = scale_ 2 (f4 - f2)

                -- result using 4-point rule
                result4 = scale_ (22 / 3) (f4 - f3)
                          - scale_ (62 / 3) (f3 - f2)
                          + scale_ (52 / 3) (f2 - f1)

                result = scale (recip h) result4

                -- The truncation error in the @result4@ approximation itself is
                -- O(h^3). For safety, we estimate the error from
                -- @result4 - result2@, which is O(h). By
                -- scaling @h@ we will minimize this estimated error, not
                -- the actual error in @result4@.
                trunc = limiting (result4 - result2) / abs h

                -- Rounding error
                round =
                    let
                        -- rounding error from the 4-point rule
                        error4 = 2 * 20.67 * sum (precision <$> fs)

                        -- rounding error due to finite precision in
                        -- x + h = O(eps * x)
                        prec =
                            let
                                prec2 = precision result2 / abs h
                                prec4 = precision result4 / abs h
                            in
                              max prec2 prec4 * abs (x / h)
                    in
                      error4 / abs h + prec
            in
              (result, trunc, round)

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
             && (errOpt < err0 && limiting (rOpt - r0) < 4.0 * err0)
      then (rOpt, errOpt)
      else (r0, err0)

backward :: ( Floating x, Ord x
            , Num r, Imprecise r, Estimate r ~ x ) =>
            (forall t. Traversable t => t x -> t r)
                -- ^ the function to differentiate
         -> x  -- ^ evaluate the derivative at @x@
         -> x  -- ^ initial step size
         -> (r, Estimate r)  -- ^ result and error
backward f x h0 = forward f x (negate h0)
