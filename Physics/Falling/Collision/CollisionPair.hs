{-# LANGUAGE ExistentialQuantification #-}

module Physics.Falling.Collision.CollisionPair
(
CollisionPair(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Collision.Detection.CollisionDetectionAlgorithm

data (UnitVector v n,
      CollisionDetectionAlgorithm a m v n) =>
      CollisionPair a m v n = CollisionPair
                            {
                              ida                           :: Int
                              , idb                         :: Int
                              , collisionDetectionAlgorithm :: a
                            }
