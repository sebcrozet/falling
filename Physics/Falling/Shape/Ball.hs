{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.Ball
(
Ball(..)
, ball
, ballSupportPoint
, ballVolume
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.TransformableShape

data Vector v => Ball v = Ball v Double
                          deriving(Show)

ball :: Vector v => Double -> Ball v
ball radius = Ball zero radius

instance (DotProd v, Vector v) => ImplicitShape (Ball v) v where
  supportPoint = ballSupportPoint

-- this does not support scaling
instance (Transform t v) => TransformableShape (Ball v) t (Ball v) where
  transformShape t (Ball center radius) = Ball (t `transform` center) radius

instance Vector v => Translatable (Ball v) v where
  translation (Ball v _)    = v
  translate   dv (Ball v r) = Ball (v &+ dv) r

ballSupportPoint :: (DotProd v, Vector v) => (Ball v) -> v -> v
ballSupportPoint (Ball center radius) direction = center &+ (direction &* (1.0 / len direction)) &* radius

ballVolume :: Vector v => (Ball v) -> Int -> Double
ballVolume (Ball _ radius) dimension = pi * radius ^ dimension
