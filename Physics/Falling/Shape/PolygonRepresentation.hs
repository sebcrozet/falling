module Physics.Falling.Shape.PolygonRepresentation
where

-- import Data.List.Extras.Argmax
import Data.Vect.Double.Base
import Data.Vect.Double.Util.Dim2

data PolygonRepresentation = LineStrip  [ Vec2 ]
                           | DKHierarchy -- FIXME

-- instance ImplicitShape PolygonRepresentation Vec2 where
--   supportPoint dir (LineStrip points) = argmax (dir &.) points
-- 
--   supportPoint _ DKHierarchy        = error "Dobkin Kickpatrick Hierarchy "
--                                             "is not yet implemented."
-- 
-- instance VolumetricShape PolygonRepresentation where
--   volume (LineStrip points) = pointsSetArea points
-- 
--   volume DKHierarchy        = error "Dobkin Kickpatrick Hierarchy "
--                                     "is not yet implemented."

pointsSetArea :: [ Vec2 ] -> Double
pointsSetArea pts = abs . (/ 2.0) $ foldl1 (+) $ doubleMap det2 closedPolygon
                    where
                    closedPolygon = (pts !! 0) : pts
                    doubleMap f (a:b:l) = (f a b) : doubleMap f (b:l)
                    doubleMap _ _       = []
