module Physics.Falling.Collision.Detection.IncrementalContactManifold
(
addContact
, updateContacts
, updateContact
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Collision.Collision

updateContacts :: (Transform m v, UnitVector v n) =>
                  (m, m) -> (m, m) -> [CollisionDescr v n] -> [CollisionDescr v n]
updateContacts t1s t2s = filter ((>= 0.0) . penetrationDepth) . map (updateContact t1s t2s)

updateContact :: (Transform m v, UnitVector v n) =>
                 (m, m) -> (m, m) -> CollisionDescr v n -> CollisionDescr v n
updateContact (t1, it1) (t2, it2) (CollisionDescr _ lp1 lp2 n d) = mkCollisionDescrWithCenter it1 it2 c' n d'
                                                                   where
                                                                   wp1  = t1 `transform` lp1
                                                                   wp2  = t2 `transform` lp2
                                                                   c'   = (wp1 &+ wp2) &* 0.5
                                                                   d'   = d - (wp2 &- wp1) &. (fromNormal n)

addContact :: (UnitVector v n, Dimension v) =>
              CollisionDescr v n -> [CollisionDescr v n] -> [CollisionDescr v n]
addContact c cs = if length cs < 2 * (dim (contactCenter c) - 1) then
                    c : cs
                  else
                    c : init cs -- FIXME: _addContactMatch c cs
