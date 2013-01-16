{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Shape.Ball
(
Ball(..)
, ballSupportPoint
, ballVolume
)
where

import Data.Vect.Double.Base

newtype Ball = Ball Double

ballSupportPoint :: (Vector v) => v -> Ball -> v
ballSupportPoint direction (Ball radius) = direction &* radius

ballVolume :: Ball -> Int -> Double
ballVolume (Ball radius) dimension = pi * radius ^ dimension
