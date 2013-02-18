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

-- | A geometric object with a n-dimensional ball shape. A ball has a center and a radius. It is
-- assumed that the ball is not empty. Its dimension is the same as its center vector
-- dimension.
data Vector v => Ball v = Ball v Double
                          deriving(Show)

-- | Creates a ball centered at the origin with a given radius.
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

-- | Support function of the ball. 
ballSupportPoint :: (DotProd v, Vector v) => (Ball v) -> v -> v
ballSupportPoint (Ball center radius) direction = center &+ (direction &* (1.0 / len direction)) &* radius

-- | Volume of the ball.
ballVolume :: (Dimension v, Vector v) => (Ball v) -> Double
ballVolume (Ball c radius) = pi * radius ^ (dim c)
