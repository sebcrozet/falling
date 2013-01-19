{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.Ball
(
Ball(..)
, ballSupportPoint
, ballVolume
)
where

import Data.Vect.Double.Base
import Physics.Falling.Shape.ImplicitShape

newtype Ball = Ball Double

instance (Vector v) => ImplicitShape Ball v where
  supportPoint = ballSupportPoint

ballSupportPoint :: (Vector v) => v -> Ball -> v
ballSupportPoint direction (Ball radius) = direction &* radius

ballVolume :: Ball -> Int -> Double
ballVolume (Ball radius) dimension = pi * radius ^ dimension
