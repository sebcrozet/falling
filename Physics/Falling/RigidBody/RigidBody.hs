{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.RigidBody.RigidBody
(
RigidBody(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.Shape.TransformableShape
import Physics.Falling.Integrator.Integrable
import Physics.Falling.RigidBody.DynamicBody
import Physics.Falling.RigidBody.StaticBody
import Physics.Falling.Integrator.GenericEulerIntegrator
import Physics.Falling.Identification.IndexGenerator
import qualified Physics.Falling.Identification.SignedIndexGenerator as IG

data (TransformSystem transformType linearVelocityType angularVelocityType
      , TransformableShape dynamicCollisionVolumeType transformType transformedDynamicCollisionVolumeType
      , TransformableShape staticCollisionVolumeType  transformType transformedStaticCollisionVolumeType
      , VolumetricShape dynamicCollisionVolumeType
                        inertiaTensorType
                        inverseInertiaTensorType
                        angularVelocityType
                        transformType
     ) =>
     RigidBody transformType
               linearVelocityType
               angularVelocityType
               inertiaTensorType
               inverseInertiaTensorType
               dynamicCollisionVolumeType
               staticCollisionVolumeType
               transformedDynamicCollisionVolumeType
               transformedStaticCollisionVolumeType
               = DynamicBody (DynamicBody transformType
                                          linearVelocityType
                                          angularVelocityType
                                          inertiaTensorType
                                          inverseInertiaTensorType
                                          dynamicCollisionVolumeType
                                          transformedDynamicCollisionVolumeType)
                 | StaticBody (StaticBody transformType
                                          linearVelocityType
                                          angularVelocityType
                                          staticCollisionVolumeType
                                          transformedStaticCollisionVolumeType)
                 deriving(Show)
-- | KinematicBody KinematicBody -- FIXME
-- | PhantomBody   PhantomBody   -- FIXME

instance (TransformSystem      t   lv av
          , VolumetricShape    dvt i  ii av t
          , TransformableShape dvt t  dvt'
          , TransformableShape svt t  svt') =>
         Integrable (RigidBody t lv av i ii dvt svt dvt' svt') where
  integrateVelocity dt (DynamicBody d) = DynamicBody $ integrateBodyVelocity dt d
  integrateVelocity _  b               = b
  
  integratePosition dt (DynamicBody d) = DynamicBody $ integrateBodyPosition dt d
  integratePosition _  b               = b

instance (TransformSystem      t   lv av
          , VolumetricShape    dvt i  ii av t
          , TransformableShape dvt t  dvt'
          , TransformableShape svt t  svt') =>
         IndexGenerator IG.SignedIndexGenerator (RigidBody t lv av i ii dvt svt dvt' svt') where
  generate (DynamicBody _) = IG.generatePositiveId
  generate _               = IG.generateNegativeId
  recycle  _               = IG.recycle
