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
                                      , Translatable  g2 v
                                      , UnitVector    v  n
                                      , Fractional    v
                                      , Eq            v
                                      , UnitSphere    n) =>
                                     g1 -> g2 -> Int -> Maybe (PartialCollisionDescr v n)
collideImplicitShapeImplicitShape g1 g2 numSamples =
                                  case closestPoints g1 g2 of
                                  Just    (p1, p2) -> if ldp > 2.0 * margin then
                                                        Nothing
                                                      else
                                                        Just $ (p1 &+ marginn
                                                                , p2 &- marginn
                                                                , toNormalUnsafe n
                                                                , (2.0 * margin - ldp))
                                                        where
                                                        dp      = p2 &- p1
                                                        ldp     = len dp
                                                        n       = dp &* (1.0 / ldp)
                                                        marginn = n &* margin
                                  Nothing          ->  _deepPenetration g1 g2 numSamples


_deepPenetration :: (Dimension       v
                     , ImplicitShape g1 v
                     , ImplicitShape g2 v
                     , Translatable  g2 v
                     , UnitVector    v  n
                     , Fractional    v
                     , Eq            v
                     , UnitSphere    n) =>
                    g1 -> g2 -> Int -> Maybe (PartialCollisionDescr v n)
_deepPenetration g1 g2 numSamples =
                 case approximatePenetration g1 g2 numSamples of
                 Nothing              -> error "Internal error: penetration should have been computed at this point."
                 Just (depth, p1, p2) -> Just $ (p1 &+ marginn, p2 &- marginn, n, depth)
                                         where
                                         n       = mkNormal (p1 &- p2)
                                         marginn = fromNormal n &* margin
