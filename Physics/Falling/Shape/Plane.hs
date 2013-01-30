module Physics.Falling.Shape.Plane
where

import Data.Vect.Double.Base

newtype (Vector v) => Plane v = Plane v
                                deriving(Show)
