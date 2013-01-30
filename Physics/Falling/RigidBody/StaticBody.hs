{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.RigidBody.StaticBody
(
StaticBody
, mkStaticBody
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.CollisionVolume

data (TransformSystem transformType vectorType angleType) =>
     StaticBody transformType
                vectorType
                angleType
                collisionVolumeType = StaticBody {
                                         localToWorld       :: transformType
                                         , worldToLocal     :: transformType
                                         , collisionVolume  :: collisionVolumeType
                                      } deriving(Show)

instance (TransformSystem t v a) => Positionable (StaticBody t v a cvt) t v a where
  getLocalToWorld = localToWorld
  getWorldToLocal = worldToLocal
  setTransforms i it body = body {
                              localToWorld = i
                              , worldToLocal = it
                            }

instance (TransformSystem t v a) => CollisionVolume (StaticBody t v a cvt) cvt where
  getCollisionVolume                         = collisionVolume
  setCollisionVolume newCollisionVolume body = body { collisionVolume = newCollisionVolume }

mkStaticBody :: (TransformSystem t v a) => t -> cvt -> StaticBody t v a cvt
mkStaticBody initLocalToWorld initCollisionVolume = StaticBody {
                                                      localToWorld = initLocalToWorld
                                                      , worldToLocal = inverse initLocalToWorld
                                                      , collisionVolume = initCollisionVolume
                                                    }
