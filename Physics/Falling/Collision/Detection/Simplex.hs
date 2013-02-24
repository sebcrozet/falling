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

{-# INLINABLE points #-}
points :: (DotProd v, Vector v) => Simplex v -> [ v ]
points (Simplex pts) = pts

emptySimplex :: (DotProd v, Vector v) => Simplex v
emptySimplex = Simplex []

dimension :: (DotProd v, Vector v) => Simplex v -> Int
dimension (Simplex vs) = length vs - 1

{-# INLINABLE contains #-}
contains :: (Eq v, DotProd v, Vector v) => v -> Simplex v -> Bool
contains v (Simplex vs) = any (== v) vs

{-# INLINABLE addPoint #-}
addPoint :: (DotProd v, Vector v) => v -> Simplex v -> Simplex v
addPoint v (Simplex vs) = Simplex (v:vs) -- FIXME: check degeneracies?

{-# INLINABLE maxLen #-}
maxLen :: (DotProd v, Vector v) => Simplex v -> Double
maxLen (Simplex vs) = maximum $ map lensqr vs

{-# INLINABLE projectOrigin #-}
projectOrigin :: (DotProd v, Vector v) => Simplex v -> (v, [ Double ], Simplex v)
projectOrigin (Simplex [])       = error "Cannot project the origin on an empty simplex."
projectOrigin s@(Simplex (v:[])) = (v, [ 1.0 ], s)
projectOrigin s@(Simplex (pts))    = (proj, barycentricCoordinates, Simplex bestPoints)
                                    where
                                    cofs       = [ (uncurry cofactor) (extract i pts) sdim
                                                   | i <- [ 0 .. sdim] ]
                                    cofsValues = map fst4 cofs
                                    sdim       = dimension s
                                    (_, bestCofactorValues, bestPoints, _) =
                                      if all (> 0.0) $ cofsValues then
                                        (undefined, map fst4 cofs, pts, undefined)
                                      else
                                        foldr1 bestProjection cofs
                                    bestDeterminant        = (sum bestCofactorValues)
                                    barycentricCoordinates = map (/ bestDeterminant) bestCofactorValues
                                    proj = projection barycentricCoordinates bestPoints

{-# INLINABLE cofactor #-}
cofactor :: (DotProd v, Vector v) => v -> [ v ] -> Int -> (Double, [ Double ], [ v ], Int)
cofactor rm [] _ = (1.0, [ 1.0 ], [ rm ], 0)
cofactor rm vs d = if all (> 0.0) subCofactorValues then
                     (cofactorValue, subCofactorValues, vs, d)
                   else
                     (cofactorValue, bestCofactorValues, bestCofactorVector, bestCofactorDim)
                   where
                   p1                = vs !! 0
                   rmp1              = p1 &- rm
                   subDimension      = d - 1
                   subCofactors      = [ (uncurry cofactor) (extract i vs) subDimension
                                         | i <- [ 0 .. subDimension ] ]
                   subCofactorValues = map fst4 subCofactors
                   cofactorValue     = sum $ zipWith (\c v -> c * (rmp1 &. v)) subCofactorValues vs
                   (_, bestCofactorValues, bestCofactorVector, bestCofactorDim) =
                     foldr1 bestProjection subCofactors

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

extract :: Int -> [a] -> (a, [a])
extract i l = let (sub1, e:sub2) = splitAt i l in (e, sub1 ++ sub2)

projection' :: Vector v => [ Double ] -> [ v ] -> v
projection' cofactors pts = foldr1 (&+) $ zipWith (&*) pts $ map (/ determinant) cofactors
                            where
                            determinant = sum cofactors

projection :: Vector v => [ Double ] -> [ v ] -> v
projection barCoords pts = foldr1 (&+) $ zipWith (*&) barCoords pts

-- FIXME: this is not really the right way to implement the Johnson subalgorithm.
-- If we had a cash of all computed cofactors, we could check the negativity of the cofactors of
-- each super-simplex.
bestProjection :: (Vector v, DotProd v) =>
                  (Double, [ Double ], [ v ], Int)
                  -> (Double, [ Double ], [ v ], Int)
                  -> (Double, [ Double ], [ v ], Int)
bestProjection o@(_, cofso, ptso, _) b@(_, cofsb, ptsb, _) =
        if lensqr po < lensqr pb then o else b
        where
        po = projection' cofso ptso
        pb = projection' cofsb ptsb
