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

-- | An n-dimensional hyperplane. An hyperplane has a center and a normal. The plane is infinite on
-- all the directions orthogonal to its normal.
data (UnitVector v n, Vector v) => Plane v n = Plane v n
                                               deriving(Show)

-- | Creates a ball with a given normal and centered at the origin.
plane :: (UnitVector v n, Vector v) => n -> Plane v n
plane  n = Plane zero n

-- | Same as 'plane' but accepting a not-normalized vector as the normal.
planev :: (UnitVector v n, Vector v) => v -> Plane v n
planev v = Plane zero $ mkNormal v

instance (Transform t v, UnitVector v n) => TransformableShape (Plane v n) t (Plane v n) where
  transformShape t (Plane v n) = Plane (t `transform` v) $ mkNormal (t `deltaTransform` fromNormal n)
