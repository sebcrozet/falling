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
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.Integrator.Integrable
import qualified Physics.Falling.Identification.Identifiable as I
import Physics.Falling.Identification.IndexGenerator
import qualified Physics.Falling.Identification.SignedIndexGenerator as IG

data (Ord identifierType
      , Transform transformType linearVelocityType angularVelocityType
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
                      = OrderedRigidBody {
                         identifier  :: identifierType
                         , rigidBody :: RigidBody transformType
                                                  linearVelocityType
                                                  angularVelocityType
                                                  inertiaTensorType
                                                  inverseInertiaTensorType
                                                  dynamicCollisionVolumeType
                                                  staticCollisionVolumeType
                        }

instance (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t)
         => I.Identifiable (OrderedRigidBody idt t lv av i ii dvt svt) idt where
  identifier = identifier

instance (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t)
         => Integrable (OrderedRigidBody idt t lv av i ii dvt svt) where
  integrateVelocity dt = mapOnBody $ integrateVelocity dt
  integratePosition dt = mapOnBody $ integratePosition dt

instance (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t)
         => IndexGenerator IG.SignedIndexGenerator (OrderedRigidBody idt t lv av i ii dvt svt) where
  generate b   = generate $ rigidBody b
  recycle  _   = IG.recycle

instance (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t)
         => Eq (OrderedRigidBody idt t lv av i ii dvt svt) where
  a == b = identifier a == identifier b

instance (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t)
         => Ord (OrderedRigidBody idt t lv av i ii dvt svt) where
  a <= b = identifier a <= identifier b

orderRigidBody :: (Ord idt, Transform t lv av, VolumetricShape dvt i ii av t) =>
                  idt -> RigidBody t lv av i ii dvt svt -> OrderedRigidBody  idt t lv av i ii dvt svt
orderRigidBody i b = OrderedRigidBody {
                       identifier = i
                       , rigidBody = b
                     }

mapOnBody :: (Ord idt, Transform t lv av, VolumetricShape dvt i ii av t) =>
             (RigidBody t lv av i ii dvt svt -> RigidBody t lv av i ii dvt svt) ->
             OrderedRigidBody  idt t lv av i ii dvt svt ->
             OrderedRigidBody  idt t lv av i ii dvt svt
mapOnBody f ob = ob { rigidBody = f $ rigidBody ob }
