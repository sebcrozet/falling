{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.RigidBody.DynamicBody
(
DynamicBody
, mkDynamicBody
)
where

import Data.Vect.Double.Base
import Physics.Falling.Dynamics.InertiaTensor
import Physics.Falling.Math.Transform
import qualified Physics.Falling.Shape.VolumetricShape as VS
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.RigidBody.CollisionVolume

data (TransformSystem transformType linearVelocityType angularVelocityType
      , VS.VolumetricShape collisionVolumeType
                           inertiaTensorType
                           inverseInertiaTensorType
                           angularVelocityType
                           transformType) =>
     DynamicBody transformType
                 linearVelocityType
                 angularVelocityType
                 inertiaTensorType
                 inverseInertiaTensorType
                 collisionVolumeType
                 = DynamicBody
                   {
                      localToWorld                      :: transformType
                      , worldToLocal                    :: transformType
                      , linearVelocity                  :: linearVelocityType
                      , angularVelocity                 :: angularVelocityType
                      , volume                          :: Double
                      , inverseMass                     :: Double
                      , inverseInertiaTensor            :: inverseInertiaTensorType
                      , worldSpaceInverseInertiaTensor  :: inverseInertiaTensorType
                      , collisionVolume                 :: collisionVolumeType
                      , externalLinearForce             :: linearVelocityType
                      , externalAngularForce            :: angularVelocityType
                   } deriving(Show)

instance (TransformSystem t lv av, VS.VolumetricShape cvt i ii av t) =>
         Positionable (DynamicBody t lv av i ii cvt) t lv av where
  getLocalToWorld = localToWorld
  getWorldToLocal = worldToLocal
  setTransforms t it body = body {
                              localToWorld                     = t
                              , worldToLocal                   = it
                              , worldSpaceInverseInertiaTensor = newWorldInverseTensor
                            }
                            where
                            newWorldInverseTensor = toWorldSpaceTensor (inverseInertiaTensor body)
                                                                       t
                                                                       it

instance (TransformSystem t lv av, VS.VolumetricShape cvt i ii av t) =>
         CollisionVolume (DynamicBody t lv av i ii cvt) cvt where
  getCollisionVolume                         = collisionVolume
  setCollisionVolume newCollisionVolume body = _updateMassAndTensor $ body { collisionVolume = newCollisionVolume }

instance (TransformSystem t lv av, VS.VolumetricShape cvt i ii av t) =>
         Dynamic (DynamicBody t lv av i ii cvt) t lv av ii where
  getLinearVelocity                 = linearVelocity
  getAngularVelocity                = angularVelocity
  getInverseMass                    = inverseMass
  getWorldSpaceInverseInertiaTensor = worldSpaceInverseInertiaTensor
  getExternalLinearForce            = externalLinearForce
  getExternalAngularForce           = externalAngularForce
  setExternalLinearForce f body     = body { externalLinearForce = f }
  setExternalAngularForce f body    = body { externalAngularForce = f }
  setLinearVelocity  linVel body    = body { linearVelocity = linVel }
  setAngularVelocity angVel body    = body { angularVelocity = angVel }

mkDynamicBody :: (TransformSystem t lv av, VS.VolumetricShape cvt i ii av t) =>
                 t -> cvt -> Double -> lv -> av -> DynamicBody t lv av i ii cvt 
mkDynamicBody initLocalToWorld
              initCollisionVolume
              initVolume
              initLinearVelocity
              initAngularVelocity =
              setCollisionVolume initCollisionVolume $ DynamicBody
                                                       {
                                                         localToWorld                     = initLocalToWorld
                                                         , worldToLocal                   = inverse initLocalToWorld
                                                         , linearVelocity                 = initLinearVelocity
                                                         , angularVelocity                = initAngularVelocity
                                                         , volume                         = initVolume
                                                         , externalLinearForce            = zero
                                                         , externalAngularForce           = zero
                                                         , collisionVolume                = undefined
                                                         , inverseMass                    = undefined
                                                         , inverseInertiaTensor           = undefined
                                                         , worldSpaceInverseInertiaTensor = undefined
                                                       }

-- hidden functions
_updateMassAndTensor :: (TransformSystem t lv av, VS.VolumetricShape cvt i ii av t) =>
                        DynamicBody t lv av i ii cvt -> DynamicBody t lv av i ii cvt
_updateMassAndTensor b = b {
                           inverseMass                      = 1.0 / mass
                           , inverseInertiaTensor           = inverseTensor
                           , worldSpaceInverseInertiaTensor = toWorldSpaceTensor inverseTensor
                                                                                 (localToWorld b)
                                                                                 (worldToLocal b)
                         }
                         where
                         mass          = volume b * VS.volume shape
                         inverseTensor = VS.objectFrameInverseInertiaTensor shape mass
                         shape         = collisionVolume b
