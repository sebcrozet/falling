module Physics.Falling.Collision.Detection.PlaneImplicitShapeCollisionDetector
(
collidePlaneImplicitShape
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.Plane
import Physics.Falling.Collision.Collision

collidePlaneImplicitShape :: (ImplicitShape g v, UnitVector v n) =>
                             Plane v n -> g -> Maybe (PartialCollisionDescr v n)
collidePlaneImplicitShape (Plane center planeNormal) other =
                          if d > 0.0 then
                            Just $ (cp1, cp2, n, d)
                          else
                            Nothing
                          where
                          invPlaneNormal = neg $ fromNormal planeNormal
                          deepestPoint   = supportPoint other invPlaneNormal
                          n              = planeNormal
                          d              = (center  &- deepestPoint) &. fromNormal planeNormal
                          cp1            = deepestPoint &+ fromNormal planeNormal &* d
                          cp2            = deepestPoint
