module Physics.Falling.Collision.Collision
(
CollisionDescr(..)
, PartialCollisionDescr
, Collision(..)
, ContactManifold
, contactManifoldGeometries
, collisionDescr2UnibodyCollision
, collisionDescr2BibodyCollision
, revertCollisionDescr
, mkCollisionDescr
)
where

import Physics.Falling.Math.Transform

type PartialCollisionDescr v n = (v, v, n, Double)

data (Vector v, UnitVector v n) => CollisionDescr v n = CollisionDescr
                                                        {
                                                          contactCenter      :: v
                                                          , localContact1    :: v
                                                          , localContact2    :: v
                                                          , contactNormal    :: n
                                                          , penetrationDepth :: Double
                                                        } deriving(Show)

data (Vector v, UnitVector v n) => Collision v n = UnibodyCollision
                                                   {
                                                     bodyIndex           :: Int
                                                     , collisionGeometry :: CollisionDescr v n
                                                     , impulseCash       :: Double -- FIXME: remove that?
                                                   }
                                                   | BibodyCollision
                                                   {
                                                     bodyIndex1          :: Int
                                                     , bodyIndex2        :: Int
                                                     , collisionGeometry :: CollisionDescr v n
                                                     , impulseCash       :: Double -- FIXME: remove that?
                                                   }
                                                   deriving(Show)

type ContactManifold v n = [ Collision v n ]

contactManifoldGeometries :: (Vector v, UnitVector v n) => ContactManifold v n -> [CollisionDescr v n]
contactManifoldGeometries = map extractCollisionGeometry
                            where
                            extractCollisionGeometry (UnibodyCollision _ cd _)    = cd
                            extractCollisionGeometry (BibodyCollision  _ _  cd _) = cd

collisionDescr2UnibodyCollision :: (Vector v, UnitVector v n) =>
                                   Int -> CollisionDescr v n -> Collision v n
collisionDescr2UnibodyCollision idx c = UnibodyCollision idx c 0.0

collisionDescr2BibodyCollision :: (Vector v, UnitVector v n) =>
                                  Int -> Int -> CollisionDescr v n -> Collision v n
collisionDescr2BibodyCollision id1 id2 c = BibodyCollision id1 id2 c 0.0

revertCollisionDescr :: (Vector v, UnitVector v n) => CollisionDescr v n -> CollisionDescr v n
revertCollisionDescr (CollisionDescr v lv1 lv2 n d) = CollisionDescr v lv2 lv1 (toNormalUnsafe $ neg $ fromNormal n) d

mkCollisionDescr :: (Vector v, UnitVector v n, Transform m v) =>
                    m -> m -> PartialCollisionDescr v n -> CollisionDescr v n
mkCollisionDescr it1 it2 (pt1, pt2, n, d) =
                 CollisionDescr ((pt1 &+ pt2) &* 0.5) (it1 `transform` pt1) (it2 `transform` pt2) n d
                 -- FIXWE: we could compute d here
