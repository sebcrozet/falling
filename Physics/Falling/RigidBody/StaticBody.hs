{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.RigidBody.StaticBody
(
StaticBody
, mkStaticBody
)
where

import Physics.Falling.Math.Transform
import qualified Physics.Falling.RigidBody.Positionable    as P
import qualified Physics.Falling.RigidBody.CollisionVolume as C

data (TransformSystem transformType vectorType angleType) =>
     StaticBody transformType
                vectorType
                angleType
                collisionVolumeType = StaticBody {
                                         localToWorld       :: transformType
                                         , worldToLocal     :: transformType
                                         , collisionVolume  :: collisionVolumeType
                                      } deriving(Show)

instance (TransformSystem t v a) => P.Positionable (StaticBody t v a cvt) t v a where
  localToWorld            = localToWorld
  worldToLocal            = worldToLocal
  setTransforms i it body = body {
                              localToWorld = i
                              , worldToLocal = it
                            }

instance (TransformSystem t v a) => C.CollisionVolume (StaticBody t v a cvt) cvt where
  collisionVolume                            = collisionVolume
  setCollisionVolume newCollisionVolume body = body { collisionVolume = newCollisionVolume }

mkStaticBody :: (TransformSystem t v a) => t -> cvt -> StaticBody t v a cvt
mkStaticBody initLocalToWorld initCollisionVolume = StaticBody {
                                                      localToWorld = initLocalToWorld
                                                      , worldToLocal = inverse initLocalToWorld
                                                      , collisionVolume = initCollisionVolume
                                                    }
