{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.Plane
(
Plane(..)
, plane
, planev
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.TransformableShape

data (UnitVector v n, Vector v) => Plane v n = Plane v n
                                               deriving(Show)

plane :: (UnitVector v n, Vector v) => n -> Plane v n
plane  n = Plane zero n

planev :: (UnitVector v n, Vector v) => v -> Plane v n
planev v = Plane zero $ mkNormal v

instance (Transform t v, UnitVector v n) => TransformableShape (Plane v n) t (Plane v n) where
  transformShape t (Plane v n) = Plane (t `transform` v) $ mkNormal (t `deltaTransform` fromNormal n)
