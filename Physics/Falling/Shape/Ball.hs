{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.Ball
(
Ball(..)
, ballSupportPoint
, ballVolume
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape

newtype Vector v => Ball v = Ball Double
                             deriving(Show)

instance (DotProd v, Vector v) => ImplicitShape (Ball v) v where
  supportPoint = ballSupportPoint

ballSupportPoint :: (DotProd v, Vector v) => (Ball v) -> v -> v
ballSupportPoint (Ball radius) direction = (direction &* (1.0 / len direction)) &* radius

ballVolume :: (Ball v) -> Int -> Double
ballVolume (Ball radius) dimension = pi * radius ^ dimension
