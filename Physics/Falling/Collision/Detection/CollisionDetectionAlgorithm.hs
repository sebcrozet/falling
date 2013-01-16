{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Collision.Detection.CollisionDetectionAlgorithm
(
CollisionDetectionAlgorithm(..)
)
where

import Data.Maybe
import Data.Vect.Double.Base
import Physics.Falling.Collision.Collision

class (UnitVector v n) => CollisionDetectionAlgorithm a t v n | v -> n,
                                                                n -> v,
                                                                a -> t,
                                                                a -> v,
                                                                a -> n
                                                                where
  getCollisions :: a -> t -> t -> (a, Maybe [ CollisionDescr v n ])
  testCollision :: a -> t -> t -> (a, Bool)

  testCollision algo t1 t2 = (\(a1, a2) -> (a1, isNothing a2)) $ getCollisions algo t1 t2
