module Physics.Falling.Collision.Detection.MinkowskiSampling
(
approximatePenetration
, approximatePenetrationWithDirections
)
where

import Data.List
import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform
import Physics.Falling.Math.UnitSphere
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.ShapeWithMargin
import Physics.Falling.Shape.CSO
import Physics.Falling.Collision.Detection.GJK

approximatePenetration:: (Dimension       v
                          , Translatable  g2 v
                          , ImplicitShape g1 v
                          , ImplicitShape g2 v
                          , UnitVector    v  n
                          , Fractional    v
                          , UnitSphere    n
                          , Eq            v) =>
                          g1 -> g2 -> Int -> Maybe (Double, v, v)
approximatePenetration g1 g2 numSamples =
                       approximatePenetrationWithDirections g1 g2 (nUnitSphereSamples numSamples)

approximatePenetrationWithDirections :: (Dimension       v
                                         , Translatable  g2 v
                                         , ImplicitShape g1 v
                                         , ImplicitShape g2 v
                                         , UnitVector    v  n
                                         , Fractional    v
                                         , Eq            v) =>
                                         g1 -> g2 -> [ n ] -> Maybe (Double, v, v)
approximatePenetrationWithDirections g1 g2 dirs = 
  if candidateDepth < 0.0 then
    error "Internal error: impossible candidateDepth"
  else
    Just (depth, p1', p2' &- shift')
  where
  mg1           = ShapeWithMargin g1 margin
  mg2           = ShapeWithMargin g2 margin
  cso           = mkCSO mg1 mg2
  (canditateNormal, candidateDepth) = minimumBy (\a b -> compare (snd a) (snd b))
                                      $ map (\d -> let dv = fromNormal d in (dv, supportPoint cso dv &. dv))
                                        dirs
  shift           = canditateNormal &* candidateDepth
  Just (p1, p2)   = closestPoints g1 $ translate shift g2
  finalNormal     = fromNormal $ mkNormal $ p2 &- p1
  depth           = finalNormal &. supportPoint cso finalNormal
  shift'          = finalNormal &* depth
  Just (p1', p2') = closestPoints g1 $ translate shift' g2
