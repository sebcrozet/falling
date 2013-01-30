{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Collision.Detection.CollisionDetectionDispatcher
(
CollisionDispatcher(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Collision.Detection.CollisionDetectionAlgorithm

class (UnitVector v n, CollisionDetectionAlgorithm a m v n) => CollisionDispatcher d a g m v n where
  mkCollisionAlgorithm :: CollisionDetectionAlgorithm a m v n => d -> g -> g -> a
