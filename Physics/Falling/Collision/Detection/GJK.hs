module Physics.Falling.Collision.Detection.GJK
(
distance
)
where

import Data.Vect.Double.Base hiding(translation, distance)
import Physics.Falling.Math.Transform
import Physics.Falling.Math.Error
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Collision.Detection.Simplex hiding(dimension)
import qualified Physics.Falling.Collision.Detection.Simplex as S (dimension)

type SimplexResult v = Either (v, [ Double ], Simplex v, Simplex v) -- need more iterations
                              (v, [ Double ], Simplex v)            -- success

distance :: (ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n, Eq v, Fractional v) =>
            (g1, m, m) -> (g2, m, m) -> Int -> Double
distance s1 s2 dimension = case algorithmGJK s1 s2 dimension $ Left (initialPoint, [], emptySimplex, emptySimplex) of
                           Nothing                 -> 0.0
                           Just (projection, _, _) -> len projection
                           where
                           snd3 (_, v, _)   = v
                           initialDirection = translation (snd3 s1) &- translation (snd3 s2)
                           notNullDirection = if lensqr initialDirection /= 0.0 then translation (snd3 s1)
                                              else 1.0
                           initialPoint     = sampleCSO s1 s2 $ mkNormal notNullDirection

-- collisionWithMargin margin s1 s2 dimension = 

algorithmGJK :: (ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n, Eq v) =>
                (g1, m, m) -> (g2, m, m) -> Int -> SimplexResult v -> Maybe (v, Simplex v, [ Double ])
algorithmGJK s1 s2 dimension (Left (projection, barCoords, lastSimplex, newSimplex))
             | sDim /= 0 && (sDim == dimension + 1 || lensqr projection <= epsTol * maxLen newSimplex) = Nothing
             | otherwise = algorithmGJK s1 s2 dimension $ stepGJK s1 s2 lastSimplex newSimplex projection barCoords
             where
             sDim = S.dimension newSimplex
algorithmGJK _ _ _ (Right (projection, barCoords, simplex)) = 
             Just (projection, simplex, barCoords)

stepGJK :: (ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n, Eq v) =>
           (g1, m, m) -> (g2, m, m) -> Simplex v -> Simplex v -> v -> [ Double ] -> SimplexResult v
stepGJK s1 s2 lastSimplex newSimplex v barCoords =
        if contains csoPoint lastSimplex || sqlenv - v &. csoPoint <= sqEpsRel * sqlenv then
          Right $ (v, barCoords, newSimplex) -- terminate the algorithm: distance has acceptable precision
        else
          Left $ catTuple4 newSimplex' (projectOrigin newSimplex')
        where
        sqlenv                 = lensqr v
        oppnv                  = mkNormal $ neg v
        csoPoint               = sampleCSO s1 s2 oppnv
        newSimplex'            = addPoint csoPoint newSimplex
        catTuple4 c (a, b, d)  = (a, b, c, d)
