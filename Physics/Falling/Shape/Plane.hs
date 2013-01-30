module Physics.Falling.Shape.Plane
where

import Physics.Falling.Math.Transform

newtype (Vector v) => Plane v = Plane v
                                deriving(Show)
