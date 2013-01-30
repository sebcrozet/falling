module Physics.Falling.Collision.Detection.ImplicitShapeImplicitShapeCollisionDetector
(
collideImplicitShapeImplicitShape
)
where

import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform
import Physics.Falling.Math.UnitSphere
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Collision.Detection.GJK
import Physics.Falling.Collision.Detection.MinkowskiSampling
import Physics.Falling.Collision.Collision

collideImplicitShapeImplicitShape :: (ImplicitShape   g1 v
                                      , ImplicitShape g2 v
                                      , Transform     m  v
                                      , UnitVector    v  n
                                      , Fractional    v
                                      , Eq            v
                                      , UnitSphere    n) =>
                                     (g1, m, m) -> (g2, m, m) -> Int -> Int -> Maybe (CollisionDescr v n)
collideImplicitShapeImplicitShape s1 s2 numSamples dimension =
                                  case closestPoints s1 s2 dimension of
                                  Just    (p1, p2) -> if ldp > 2.0 * margin then
                                                        Nothing
                                                      else
                                                        Just $ CollisionDescr ((p1 &+ p2) &* 0.5)
                                                                              (toNormalUnsafe $ dp &* (1.0 / ldp))
                                                                              (2.0 * margin - ldp)
                                                        where
                                                        dp  = p2 &- p1
                                                        ldp = len dp
                                  Nothing          ->  _deepPenetration s1 s2 numSamples dimension


_deepPenetration :: (ImplicitShape   g1 v
                     , ImplicitShape g2 v
                     , Transform     m  v
                     , UnitVector    v  n
                     , Fractional    v
                     , Eq            v
                     , UnitSphere    n) =>
                    (g1, m, m) -> (g2, m, m) -> Int -> Int -> Maybe (CollisionDescr v n)
_deepPenetration s1 s2 numSamples dimension =
                 case approximatePenetration s1 s2 numSamples dimension of
                 Nothing              -> error "Internal error: penetration should have been computed at this point."
                 Just (depth, p1, p2) -> Just $ CollisionDescr ((p1 &+ p2) &* 0.5)
                                                               (mkNormal (p1 &- p2))
                                                               depth
