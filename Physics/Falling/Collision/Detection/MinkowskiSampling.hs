module Physics.Falling.Collision.Detection.MinkowskiSampling
(
approximatePenetration
, approximatePenetrationWithDirections
)
where

import Data.List
import Physics.Falling.Math.Transform
import Physics.Falling.Math.UnitSphere
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.ShapeWithMargin
import Physics.Falling.Shape.CSO
import Physics.Falling.Collision.Detection.GJK

approximatePenetration:: (ImplicitShape g1 v,
                          ImplicitShape g2 v,
                          Transform     m  v,
                          UnitVector    v  n,
                          Fractional    v,
                          UnitSphere    n,
                          Eq            v) =>
                          (g1, m, m) -> (g2, m, m) -> Int -> Int -> Maybe (Double, v, v)
approximatePenetration s1 s2 numSamples dimension =
                       approximatePenetrationWithDirections s1 s2 (nUnitSphereSamples numSamples) dimension

approximatePenetrationWithDirections :: (ImplicitShape g1 v,
                                         ImplicitShape g2 v,
                                         Transform     m  v,
                                         UnitVector    v  n,
                                         Fractional    v,
                                         Eq            v) =>
                                         (g1, m, m) -> (g2, m, m) -> [ n ] -> Int -> Maybe (Double, v, v)
approximatePenetrationWithDirections s1@(g1, t1, it1) (g2, t2, it2) dirs dimension = 
  if candidateDepth < 0.0 then
    Nothing
  else
    Just (depth, p1', p2' &- shift')
  where
  mg1           = ShapeWithMargin g1 -- FIXME: margins will be affected by scaling
  mg2           = ShapeWithMargin g2 -- FIXME: margins will be affected by scaling
  cso           = mkCSOWithTransforms (mg1, t1, it1) (mg2, t2, it2)
  (canditateNormal, candidateDepth) = minimumBy (\a b -> compare (snd a) (snd b))
                                      $ map (\d -> let dv = fromNormal d in (dv, supportPoint cso dv &. dv))
                                        dirs
  shift         = canditateNormal &* candidateDepth
  Just (p1, p2) = closestPoints s1
                                (g2, translate shift t2, translate (neg shift) it2)
                                dimension
  finalNormal     = fromNormal $ mkNormal $ p2 &- p1
  depth           = finalNormal &. supportPoint cso finalNormal
  shift'          = finalNormal &* depth
  Just (p1', p2') = closestPoints s1
                                  (g2, translate shift' t2, translate (neg shift') it2)
                                  dimension
