{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.RigidBody.OrderedRigidBody
(
OrderedRigidBody
, rigidBody
, orderRigidBody
, mapOnBody
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.Shape.TransformableShape
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.Integrator.Integrable
import qualified Physics.Falling.Identification.Identifiable as I
import Physics.Falling.Identification.IndexGenerator
import qualified Physics.Falling.Identification.SignedIndexGenerator as IG

data (Ord identifierType
      , TransformSystem transformType linearVelocityType angularVelocityType
      , TransformableShape dynamicCollisionVolumeType transformType transformedDynamicCollisionVolumeType
      , TransformableShape staticCollisionVolumeType transformType transformedStaticCollisionVolumeType
      , VolumetricShape dynamicCollisionVolumeType
                        inertiaTensorType
                        inverseInertiaTensorType
                        angularVelocityType
                        transformType
     ) =>
     OrderedRigidBody identifierType -- FIXME: put this parameter at the end
                      transformType
                      linearVelocityType
                      angularVelocityType
                      inertiaTensorType
                      inverseInertiaTensorType
                      dynamicCollisionVolumeType
                      staticCollisionVolumeType
                      transformedDynamicCollisionVolumeType
                      transformedStaticCollisionVolumeType
                      = OrderedRigidBody {
                         identifier  :: identifierType
                         , rigidBody :: RigidBody transformType
                                                  linearVelocityType
                                                  angularVelocityType
                                                  inertiaTensorType
                                                  inverseInertiaTensorType
                                                  dynamicCollisionVolumeType
                                                  staticCollisionVolumeType
                                                  transformedDynamicCollisionVolumeType
                                                  transformedStaticCollisionVolumeType
                        }
                        deriving(Show)

instance (Ord idt
          , TransformSystem t lv av
          , VolumetricShape dvt i ii av t
          , TransformableShape dvt t dvt' 
          , TransformableShape svt t svt')
         => I.Identifiable (OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') idt where
  identifier = identifier

instance (Ord idt
          , TransformSystem t lv av
          , VolumetricShape dvt i ii av t
          , TransformableShape dvt t dvt' 
          , TransformableShape svt t svt')
         => Integrable (OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') where
  integrateVelocity dt = mapOnBody $ integrateVelocity dt
  integratePosition dt = mapOnBody $ integratePosition dt

instance (Ord idt
          , TransformSystem t lv av
          , VolumetricShape dvt i ii av t
          , TransformableShape dvt t dvt' 
          , TransformableShape svt t svt')
         => IndexGenerator IG.SignedIndexGenerator (OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') where
  generate b   = generate $ rigidBody b
  recycle  _   = IG.recycle

instance (Ord idt
          , TransformSystem t lv av
          , VolumetricShape dvt i ii av t
          , TransformableShape dvt t dvt' 
          , TransformableShape svt t svt')
         => Eq (OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') where
  a == b = identifier a == identifier b

instance (Ord idt
          , TransformSystem t lv av
          , VolumetricShape dvt i ii av t
          , TransformableShape dvt t dvt' 
          , TransformableShape svt t svt')
         => Ord (OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') where
  a <= b = identifier a <= identifier b

orderRigidBody :: (Ord idt
                   , TransformSystem t lv av
                   , VolumetricShape dvt i ii av t
                   , TransformableShape dvt t dvt' 
                   , TransformableShape svt t svt') =>
                  idt -> RigidBody t lv av i ii dvt svt dvt' svt' -> OrderedRigidBody idt t lv av i ii dvt svt dvt' svt'
orderRigidBody i b = OrderedRigidBody {
                       identifier = i
                       , rigidBody = b
                     }

mapOnBody :: (Ord idt
              , TransformSystem t lv av
              , VolumetricShape dvt i ii av t
              , TransformableShape dvt t dvt' 
              , TransformableShape svt t svt') =>
             (RigidBody t lv av i ii dvt svt dvt' svt' -> RigidBody t lv av i ii dvt svt dvt' svt') ->
             OrderedRigidBody  idt t lv av i ii dvt svt dvt' svt' ->
             OrderedRigidBody  idt t lv av i ii dvt svt dvt' svt'
mapOnBody f ob = ob { rigidBody = f $ rigidBody ob }
