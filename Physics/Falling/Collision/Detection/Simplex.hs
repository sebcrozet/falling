module Physics.Falling.Collision.Detection.Simplex
(
Simplex
, emptySimplex
, dimension
, points
, contains
, addPoint
, maxLen
, projectOrigin
)
where

import Physics.Falling.Math.Transform

data (DotProd v, Vector v) => Simplex v = Simplex [ v ]

points :: (DotProd v, Vector v) => Simplex v -> [ v ]
points (Simplex pts) = pts

emptySimplex :: (DotProd v, Vector v) => Simplex v
emptySimplex = Simplex []

dimension :: (DotProd v, Vector v) => Simplex v -> Int
dimension (Simplex vs) = length vs

contains :: (Eq v, DotProd v, Vector v) => v -> Simplex v -> Bool
contains v (Simplex vs) = any (== v) vs

addPoint :: (DotProd v, Vector v) => v -> Simplex v -> Simplex v
addPoint v (Simplex vs) = Simplex (v:vs) -- FIXME: check degeneracies?

maxLen :: (DotProd v, Vector v) => Simplex v -> Double
maxLen (Simplex vs) = maximum $ map lensqr vs

projectOrigin :: (DotProd v, Vector v) => Simplex v -> (v, [ Double ], Simplex v)
projectOrigin (Simplex [])       = error "Cannot project the origin on an empty simplex."
projectOrigin s@(Simplex (v:[])) = (v , [ 1.0 ], s)
projectOrigin (Simplex (pts))    = (projection, barycentricCoordinates, Simplex bestCofactorVector)
                                   where
                                   cofs       = [ (uncurry cofactor) (extract i pts) lastPtsId
                                                  | i <- [ 0 .. lastPtsId] ]
                                   cofsValues = map fst4 cofs
                                   lastPtsId  = length pts - 1
                                   (_, bestCofactorValues, bestCofactorVector, _) =
                                     if cofsValues == [] then
                                       error "Empty cofs."
                                     else if all (> 0.0) $ cofsValues then
                                       (undefined, map fst4 cofs, pts, undefined)
                                     else
                                       foldr1 (\o@(_, _, _, depth) b@(_, _, _, best) ->
                                               if depth > best then o else b)
                                              cofs
                                   bestDeterminant        = (sum bestCofactorValues)
                                   barycentricCoordinates = map (/ bestDeterminant) bestCofactorValues
                                   projection = foldr1 (&+)
                                                $ zipWith (*&) barycentricCoordinates bestCofactorVector

cofactor :: (DotProd v, Vector v) => v -> [ v ] -> Int -> (Double, [ Double ], [ v ], Int)
cofactor _ [] _ = error "Cannot compute the cofactor of an empty matrix."

cofactor rm l@(v:[]) _ = if cofactorValue > 0.0 then
                           (cofactorValue, [ cofactorValue ], l, 1)
                         else
                           (cofactorValue, [], [], 0)
                         where
                         cofactorValue = (v &- rm) &. v
cofactor rm vs lvs     = if all (> 0.0) subCofactorValues then
                           (cofactorValue, subCofactorValues, vs, lvs)
                         else
                           (cofactorValue, bestCofactorValues, bestCofactorVector, bestCofactorLength)
                         where
                         p1                = vs !! 0
                         rmp1              = p1 &- rm
                         subFactorLength   = lvs - 1
                         subCofactors      = [ (uncurry cofactor) (extract i vs) subFactorLength
                                               | i <- [ 0 .. subFactorLength] ]
                         subCofactorValues = map fst4 subCofactors
                         cofactorValue     = sum $ zipWith (\c v -> c * (rmp1 &. v)) subCofactorValues vs
                         (_, bestCofactorValues, bestCofactorVector, bestCofactorLength) =
                           foldr1 (\o@(_, cofs, _, depth) b@(_, _, _, best) ->
                                   if cofs /= [] && depth > best then b else o)
                                  subCofactors

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

extract :: Int -> [a] -> (a, [a])
extract i l = let (sub1, e:sub2) = splitAt i l in (e, sub1 ++ sub2)
