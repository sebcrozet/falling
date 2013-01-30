module Physics.Falling.Collision.Detection.PlaneImplicitShapeCollisionDetector
(
collidePlaneImplicitShape
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.Plane
import Physics.Falling.Collision.Collision

collidePlaneImplicitShape :: (ImplicitShape  g  v
                              , Transform    m  v
                              , Transform    mp v
                              , UnitVector   v  n) =>
                             Plane v -> g -> mp -> m -> m -> Maybe (CollisionDescr v n)
collidePlaneImplicitShape (Plane upVec) other planeTransform otherTransform inverseOtherTransform =
                          if d > 0.0 then
                            Just $ CollisionDescr c n d
                          else
                            Nothing
                          where
                          planeCenter    = translation planeTransform
                          planeNormal    = mkNormal $ planeTransform `deltaTransform` upVec
                          invPlaneNormal = neg $ fromNormal planeNormal
                          deepestPoint   = supportPointWithTransform other
                                                                     invPlaneNormal
                                                                     otherTransform
                                                                     inverseOtherTransform
                          n              = planeNormal
                          c              = (deepestPoint &+ fromNormal planeNormal &* (d * 0.5))
                          d              = (planeCenter  &- deepestPoint) &. fromNormal planeNormal
