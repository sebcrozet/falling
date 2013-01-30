module Physics.Falling.Collision.Collision
(
CollisionDescr(..)
, Collision(..)
, ContactManifold
, collisionDescr2UnibodyCollision
, collisionDescr2BibodyCollision
, revertCollisionDescr
)
where

import Physics.Falling.Math.Transform

data (Vector v, UnitVector v n) => CollisionDescr v n = CollisionDescr
                                                        {
                                                          contactCenter      :: v
                                                          , contactNormal    :: n
                                                          , penetrationDepth :: Double
                                                        } deriving(Show)

data (Vector v, UnitVector v n) => Collision v n = UnibodyCollision
                                                   {
                                                     bodyIndex           :: Int
                                                     , collisionGeometry :: CollisionDescr v n
                                                     , impulseCash       :: Double
                                                   }
                                                   | BibodyCollision
                                                   {
                                                     bodyIndex1          :: Int
                                                     , bodyIndex2        :: Int
                                                     , collisionGeometry :: CollisionDescr v n
                                                     , impulseCash       :: Double
                                                   }
                                                   deriving(Show)

type ContactManifold v n = [ Collision v n ]

collisionDescr2UnibodyCollision :: (Vector v, UnitVector v n) =>
                                   Int -> CollisionDescr v n -> Collision v n
collisionDescr2UnibodyCollision idx c = UnibodyCollision idx c 0.0

collisionDescr2BibodyCollision :: (Vector v, UnitVector v n) =>
                                  Int -> Int -> CollisionDescr v n -> Collision v n
collisionDescr2BibodyCollision id1 id2 c = BibodyCollision id1 id2 c 0.0

revertCollisionDescr :: (Vector v, UnitVector v n) => CollisionDescr v n -> CollisionDescr v n
revertCollisionDescr (CollisionDescr v n d) = CollisionDescr v (toNormalUnsafe $ neg $ fromNormal n) d
