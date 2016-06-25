{-# LANGUAGE MultiParamTypeClasses #-}


module Scalar where


class Scalar s t where
    scale :: s -> t -> t
