module Physics.Falling.Collision.Collision
(
CollisionDescr(..)
, Collision(..)
, ContactManifold
, collisionDescr2UnibodyCollision
, collisionDescr2BibodyCollision

)
where

import Data.Vect.Double.Base

-- FIXME: move GeometricCollisionDescr on its own file
data (Vector v, UnitVector v n) => CollisionDescr v n = CollisionDescr
                                                        {
                                                          contactCenter      :: v
                                                          , contactNormal    :: n
                                                          , penetrationDepth :: Double
                                                        }

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

type ContactManifold v n = [ Collision v n ]

collisionDescr2UnibodyCollision :: (Vector v, UnitVector v n) =>
                                   Int -> CollisionDescr v n -> Collision v n
collisionDescr2UnibodyCollision idx c = UnibodyCollision idx c 0.0

collisionDescr2BibodyCollision :: (Vector v, UnitVector v n) =>
                                  Int -> Int -> CollisionDescr v n -> Collision v n
collisionDescr2BibodyCollision id1 id2 c = BibodyCollision id1 id2 c 0.0
