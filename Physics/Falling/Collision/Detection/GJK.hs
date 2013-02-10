module Physics.Falling.Collision.Detection.GJK
(
distanceToOrigin
, distance
, algorithmGJK
, closestPoints
, initialSimplexResult
, SimplexResult
)
where

import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform hiding(distance)
import Physics.Falling.Math.AnnotatedVector
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.CSO
import Physics.Falling.Collision.Detection.Simplex hiding(dimension)
import qualified Physics.Falling.Collision.Detection.Simplex as S (dimension)

type SimplexResult v = Either (v, [ Double ], Simplex v, Simplex v) -- need more iterations
                              (v, [ Double ], Simplex v)            -- success

initialSimplexResult :: (Dimension v, Vector v, DotProd v) => v -> SimplexResult v
initialSimplexResult initialPoint = Left (initialPoint, [], emptySimplex, emptySimplex)

distance :: (Dimension v, ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n, Eq v, Fractional v) =>
            (g1, m) -> (g2, m) -> Double
distance s1@(_, t1) s2@(_, t2) =
         distanceToOrigin cso (neg notZeroDirection)
         where
         cso              = mkCSOWithTransforms s1 s2
         initialDirection = translation t1 &- translation t2
         notZeroDirection = if lensqr initialDirection /= 0.0 then initialDirection
                                                              else 1.0

closestPoints :: (Dimension     v
                  , ImplicitShape g1 v
                  , ImplicitShape g2 v
                  , Transform     m  v
                  , UnitVector    v  n
                  , Eq            v
                  , Fractional    v) =>
                 (g1, m) -> (g2, m) -> Maybe (v, v)
closestPoints s1@(_, t1) s2@(_, t2) =
              case algorithmGJK cso $ initialSimplexResult initialPoint of
              Nothing        -> Nothing
              Just (_, b, s) -> Just $ _pointsFromAnnotatedSimplex b s 
              where
              cso              = mkAnnotatedCSOWithTransforms s1 s2
              initialDirection = translation t1 &- translation t2
              notZeroDirection = if lensqr initialDirection /= 0.0 then initialDirection
                                                                   else 1.0
              initialPoint     = supportPoint cso (AnnotatedVector notZeroDirection undefined)

distanceToOrigin :: (Dimension v, ImplicitShape g v, Eq v, DotProd v, Fractional v) =>
                    g -> v -> Double
distanceToOrigin s initialDirection =
                 case algorithmGJK s $ initialSimplexResult initialPoint of
                 Nothing                 -> 0.0
                 Just (projection, _, _) -> len projection
                 where
                 initialPoint     = supportPoint s initialDirection

algorithmGJK :: (Dimension v, ImplicitShape g v, Eq v, DotProd v) =>
                g -> SimplexResult v -> Maybe (v, [ Double ], Simplex v)
algorithmGJK s (Left (projection, barCoords, lastSimplex, newSimplex))
             | sDim /= 0 && (sDim == dimension + 1 || lensqr projection <= epsTol * maxLen newSimplex) = Nothing
             | otherwise = algorithmGJK s $ _stepGJK s lastSimplex newSimplex projection barCoords
             where
             sDim      = S.dimension newSimplex
             dimension = dim projection
algorithmGJK _ (Right (projection, barCoords, simplex)) = 
             Just (projection, barCoords, simplex)

_stepGJK :: (ImplicitShape g v, Eq v, DotProd v) =>
            g -> Simplex v -> Simplex v -> v -> [ Double ] -> SimplexResult v
_stepGJK s lastSimplex newSimplex v barCoords =
         if contains csoPoint lastSimplex
            || sqlenv - v &. csoPoint <= sqEpsRel * sqlenv
            || lensqr proj > sqlenv -- we test for inconsistancies: if the new lower bound
                                    -- is greater than the old one something went wrong.
                                    -- This should actually nether happen and might be an
                                    -- implementation bug…
         then
           Right $ (v, barCoords, newSimplex) -- terminate the algorithm: distance has acceptable precision
         else
           Left (proj, projCoords, newSimplex', projSimplex)
         where
         sqlenv                          = lensqr v
         csoPoint                        = supportPoint s $ neg v
         newSimplex'                     = addPoint csoPoint newSimplex
         (proj, projCoords, projSimplex) = projectOrigin newSimplex'

_pointsFromAnnotatedSimplex :: (Vector v, DotProd v) => [ Double ] -> Simplex (AnnotatedVector v (v, v)) -> (v, v)
_pointsFromAnnotatedSimplex coords simplex = let (pa, pb) = foldr1 (\(a, b) (a', b') -> (a &+ a', b &+ b'))
                                                            $ zipWith (\coord (a, b) -> (coord *& a, coord *& b))
                                                                      coords
                                                            $ map annotation
                                                            $ points simplex       
                                             in (pa, neg pb)

_pointFromBarycentricCoordinates :: Vector v => [ Double ] -> [ v ] -> v
_pointFromBarycentricCoordinates coords pts = foldr1 (&+) $ zipWith (\coord pt -> pt &* coord) coords pts
