{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Collision.Detection.CollisionDetectionDispatcher
(
CollisionDispatcher(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Collision.Detection.CollisionDetectionAlgorithm

class (UnitVector v n, CollisionDetectionAlgorithm a m v n) => CollisionDispatcher d a g m v n where
  mkCollisionAlgorithm :: CollisionDetectionAlgorithm a m v n => d -> g -> g -> a
