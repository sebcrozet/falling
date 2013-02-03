module Physics.Falling.Collision.Detection.ImplicitShapeImplicitShapeCollisionDetector
(
collideImplicitShapeImplicitShape
)
where

import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform hiding(distance)
import Physics.Falling.Math.UnitSphere
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Collision.Detection.GJK
import Physics.Falling.Collision.Detection.MinkowskiSampling
import Physics.Falling.Collision.Collision

collideImplicitShapeImplicitShape :: (Dimension       v
                                      , ImplicitShape g1 v
                                      , ImplicitShape g2 v
                                      , Transform     m  v
                                      , UnitVector    v  n
                                      , Fractional    v
                                      , Eq            v
                                      , UnitSphere    n) =>
                                     (g1, m, m) -> (g2, m, m) -> Int -> Maybe (CollisionDescr v n)
collideImplicitShapeImplicitShape s1@(g1, t1, it1) s2@(g2, t2, it2) numSamples =
                                  case closestPoints (g1, t1) (g2, t2) of
                                  Just    (p1, p2) -> if ldp > 2.0 * margin then
                                                        Nothing
                                                      else
                                                        Just $ mkCollisionDescrWithPoints
                                                                 it1
                                                                 it2
                                                                 p1
                                                                 p2
                                                                 (toNormalUnsafe $ dp &* (1.0 / ldp))
                                                                 (2.0 * margin - ldp)
                                                        where
                                                        dp  = p2 &- p1
                                                        ldp = len dp
                                  Nothing          ->  _deepPenetration s1 s2 numSamples


_deepPenetration :: (Dimension       v
                     , ImplicitShape g1 v
                     , ImplicitShape g2 v
                     , Transform     m  v
                     , UnitVector    v  n
                     , Fractional    v
                     , Eq            v
                     , UnitSphere    n) =>
                    (g1, m, m) -> (g2, m, m) -> Int -> Maybe (CollisionDescr v n)
_deepPenetration (g1, t1, it1) (g2, t2, it2) numSamples =
                 case approximatePenetration (g1, t1) (g2, t2) numSamples of
                 Nothing              -> error "Internal error: penetration should have been computed at this point."
                 Just (depth, p1, p2) -> Just $ mkCollisionDescrWithPoints it1
                                                                           it2
                                                                           p1
                                                                           p2
                                                                           (mkNormal (p1 &- p2))
                                                                           depth
