{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.RigidBody.StaticBody
(
StaticBody
, mkStaticBody
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.TransformableShape
import qualified Physics.Falling.RigidBody.Positionable    as P
import qualified Physics.Falling.RigidBody.CollisionVolume as C

data (TransformSystem transformType vectorType angleType
      , TransformableShape collisionVolumeType transformType transformedCollisionVolumeType) =>
     StaticBody transformType
                vectorType
                angleType
                collisionVolumeType
                transformedCollisionVolumeType = StaticBody {
                                                               localToWorld                :: transformType
                                                               , worldToLocal              :: transformType
                                                               , collisionVolume           :: collisionVolumeType
                                                               , worldSpaceCollisionVolume :: transformedCollisionVolumeType
                                                            } deriving(Show)

instance (TransformSystem t v a, TransformableShape cvt t cvt') =>
         P.Positionable (StaticBody t v a cvt cvt') t v a where
  localToWorld            = localToWorld
  worldToLocal            = worldToLocal
  setTransforms t it body = body {
                              localToWorld = t
                              , worldToLocal = it
                              , worldSpaceCollisionVolume = transformShape t $ collisionVolume body
                            }

instance (TransformSystem t v a, TransformableShape cvt t cvt') =>
         C.CollisionVolume (StaticBody t v a cvt cvt') cvt where
  collisionVolume                            = collisionVolume
  setCollisionVolume newCollisionVolume body =
                     body {
                            collisionVolume             = newCollisionVolume
                            , worldSpaceCollisionVolume = transformShape (localToWorld body) newCollisionVolume
                          }

instance (TransformSystem t v a, TransformableShape cvt t cvt') =>
         C.WorldSpaceCollisionVolume (StaticBody t v a cvt cvt') cvt' where
  worldSpaceCollisionVolume = worldSpaceCollisionVolume

mkStaticBody :: (TransformSystem t v a, TransformableShape cvt t cvt') =>
                t -> cvt -> StaticBody t v a cvt cvt'
mkStaticBody initLocalToWorld initCollisionVolume = C.setCollisionVolume initCollisionVolume
                                                    $ StaticBody {
                                                        localToWorld                = initLocalToWorld
                                                        , worldToLocal              = inverse initLocalToWorld
                                                        , collisionVolume           = undefined
                                                        , worldSpaceCollisionVolume = undefined
                                                      }
