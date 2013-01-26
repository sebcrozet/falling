module Physics.Falling.Collision.Detection.GJK
(
distanceToOrigin
, distance
, algorithmGJK
)
where

import Data.Vect.Double.Base hiding(translation, distance)
import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.CSO
import Physics.Falling.Collision.Detection.Simplex hiding(dimension)
import qualified Physics.Falling.Collision.Detection.Simplex as S (dimension)

type SimplexResult v = Either (v, [ Double ], Simplex v, Simplex v) -- need more iterations
                              (v, [ Double ], Simplex v)            -- success

distance :: (ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n, Eq v, Fractional v) =>
            (g1, m, m) -> (g2, m, m) -> Int -> Double
distance s1@(_, t1, _) s2@(_, t2, _) dimension =
         distanceToOrigin cso (neg notZeroDirection) dimension
         where
         cso              = mkCSOWithTransforms s1 s2
         initialDirection = translation t1 &- translation t2
         notZeroDirection = if lensqr initialDirection /= 0.0 then initialDirection
                                                              else 1.0

distanceToOrigin :: (ImplicitShape g v, Eq v, DotProd v, Fractional v) =>
                    g -> v -> Int -> Double
distanceToOrigin s initialDirection dimension =
                 case algorithmGJK s dimension $ Left (initialPoint, [], emptySimplex, emptySimplex) of
                 Nothing                 -> 0.0
                 Just (projection, _, _) -> len projection
                 where
                 initialPoint     = supportPoint s initialDirection

algorithmGJK :: (ImplicitShape g v, Eq v, DotProd v) =>
                g -> Int -> SimplexResult v -> Maybe (v, Simplex v, [ Double ])
algorithmGJK s dimension (Left (projection, barCoords, lastSimplex, newSimplex))
             | sDim /= 0 && (sDim == dimension + 1 || lensqr projection <= epsTol * maxLen newSimplex) = Nothing
             | otherwise = algorithmGJK s dimension $ stepGJK s lastSimplex newSimplex projection barCoords
             where
             sDim = S.dimension newSimplex
algorithmGJK _ _ (Right (projection, barCoords, simplex)) = 
             Just (projection, simplex, barCoords)

stepGJK :: (ImplicitShape g v, Eq v, DotProd v) =>
           g -> Simplex v -> Simplex v -> v -> [ Double ] -> SimplexResult v
stepGJK s lastSimplex newSimplex v barCoords =
        if contains csoPoint lastSimplex || sqlenv - v &. csoPoint <= sqEpsRel * sqlenv then
          Right $ (v, barCoords, newSimplex) -- terminate the algorithm: distance has acceptable precision
        else
          Left $ catTuple4 newSimplex' (projectOrigin newSimplex')
        where
        sqlenv                 = lensqr v
        csoPoint               = supportPoint s $ neg v
        newSimplex'            = addPoint csoPoint newSimplex
        catTuple4 c (a, b, d)  = (a, b, c, d)
