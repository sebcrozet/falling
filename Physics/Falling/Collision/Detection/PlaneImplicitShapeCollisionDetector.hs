module Physics.Falling.Collision.PlaneImplicitShapeCollisionDetector
(
collidePlaneImplicitShape
)
where


import Data.Vect.Double.Base
import Physics.Falling.Shape.ExtraTransform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.Plane
import Physics.Falling.Collision.Collision

collidePlaneImplicitShape :: (ImplicitShape g v,
                              DeltaTransform m v,
                              DeltaTransform mp v,
                              Position mp v,
                              AbelianGroup n,
                              LeftModule m v,
                              LeftModule mp v,
                              UnitVector v n) =>
                              Plane v -> g -> mp -> m -> m -> Maybe (GeometricCollisionDescr v n)
collidePlaneImplicitShape (Plane upVec) other planeTransform otherTransform inverseOtherTransform =
                             if d > 0.0 then
                               Just $ GeometricCollisionDescr c n d
                             else
                               Nothing
                             where
                             planeCenter    = position planeTransform
                             planeNormal    = mkNormal $ planeTransform `deltaTransform` upVec
                             invPlaneNormal = neg planeNormal
                             deepestPoint   = supportPointWithTransform other
                                                                        invPlaneNormal
                                                                        otherTransform
                                                                        inverseOtherTransform
                             n              = planeNormal
                             c              = (deepestPoint &+ planeCenter)  &* 0.5
                             d              = (planeCenter  &- deepestPoint) &. fromNormal planeNormal
