module Physics.Falling.Collision.Detection.IncrementalContactManifold
(
addContact
, updateContacts
, updateContact
)
where

import Physics.Falling.Math.Error
import Physics.Falling.Math.Transform
import Physics.Falling.Math.OrthonormalBasis
import Physics.Falling.Collision.Collision

import Control.Exception

updateContacts :: (Transform m v, UnitVector v n) =>
                  m -> m -> [CollisionDescr v n] -> [CollisionDescr v n]
updateContacts t1 t2 = map (\(Just c) -> c) . filter isJust . map (updateContact t1 t2)
                       where
                       isJust Nothing = False
                       isJust _       = True

updateContact :: (Transform m v, UnitVector v n) =>
                 m -> m -> CollisionDescr v n -> Maybe (CollisionDescr v n)
updateContact t1 t2 coll@(CollisionDescr _ lp1 lp2 n _) =
              if d' < 0.0 || lensqr terr > gap * gap then
                Nothing
              else
                Just newcoll
              where
              nv      = fromNormal n
              wp1     = t1 `transform` lp1
              wp2     = t2 `transform` lp2
              n'      = wp1 &- wp2
              d'      = n' &. nv
              terr    = n' &- nv &* d'
              newcoll = coll { penetrationDepth = d' }

addContact :: (OrthonormalBasis v n, Dimension v) =>
              CollisionDescr v n -> [CollisionDescr v n] -> [CollisionDescr v n]
addContact c cs = if length cs < 2 * (dim (contactCenter c) - 1) + 1 then
                    c : cs
                  else
                    c : _reduceContactsList cs

_reduceContactsList :: (Vector v, UnitVector v n, OrthonormalBasis v n) =>
                       [ CollisionDescr v n ] -> [ CollisionDescr v n ]
_reduceContactsList cs = map snd $ _removeDup bestPoints
                         where
                         ics               = _indexedVect cs 1
                         averageNormal     = mkNormal $ foldr sumNormals zero cs
                         sumNormals n curr = curr &+ (fromNormal $ contactNormal n)
                         hyperplanev       = map fromNormal $ snd $ completeBasis averageNormal
                         projections       = map (\v -> map (&. v) (map contactCenter cs))
                                                 hyperplanev
                         bestPoints        = map snd $ map (\p -> assert (length ics /= 0 && length p /= 0)
                                                                  $ foldr1
                                                                    (\a@(pt, _) b@(cpt, _) ->
                                                                      if pt > cpt then a else b)
                                                                    $ zip p ics)
                                                            projections

_indexedVect :: [ a ] -> Int -> [ (Int, a) ]
_indexedVect []     _ = []
_indexedVect (c:cs) i = (i, c) : _indexedVect cs (i + 1)

_removeDup :: [ (Int, a) ] -> [ (Int, a) ]
_removeDup []              = []
_removeDup ((e@(i, _)):cs) = if any ((== i).fst) cs then
                               _removeDup cs
                             else
                               e:_removeDup cs
