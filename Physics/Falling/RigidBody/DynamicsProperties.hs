module Physics.Falling.RigidBody.DynamicsProperties
where

import Data.Vect.Double.Base
import Physics.Falling.Dynamics.InertiaTensor

data (Vector v, InverseInertiaTensor v ii t) =>
     DynamicsProperties v ii t = DynamicsProperties
                                {
                                  mass                           :: Double,
                                  inverseInertiaTensor           :: ii,
                                  worldSpaceInverseInertiaTensor :: ii,
                                  linearVelocity                 :: v,
                                  angularVelocity                :: v
                                }

setMass :: (Vector v, InverseInertiaTensor v ii t) =>
           DynamicsProperties v ii t -> Double -> DynamicsProperties v ii t
setMass dynamicsProperties newMass = dynamicsProperties { mass = newMass }

setInverseInertiaTensor :: (Vector v, InverseInertiaTensor v ii t) =>
                           DynamicsProperties v ii t -> ii -> DynamicsProperties v ii t
setInverseInertiaTensor dynamicsProperties newWorldSpaceInverseInertiaTensor =
                        dynamicsProperties { worldSpaceInverseInertiaTensor = newWorldSpaceInverseInertiaTensor }

setWorldSpaceInverseInertiaTensor :: (Vector v, InverseInertiaTensor v ii t) =>
                   DynamicsProperties v ii t -> ii -> DynamicsProperties v ii t
setWorldSpaceInverseInertiaTensor dynamicsProperties newWorldSpaceInverseInertiaTensor =
                                  dynamicsProperties { worldSpaceInverseInertiaTensor = newWorldSpaceInverseInertiaTensor }

setLinearVelocity :: (Vector v, InverseInertiaTensor v ii t) =>
                   DynamicsProperties v ii t -> v -> DynamicsProperties v ii t
setLinearVelocity dynamicsProperties newVelocity = dynamicsProperties { linearVelocity = newVelocity }

setAngularVelocity :: (Vector v, InverseInertiaTensor v ii t) =>
                   DynamicsProperties v ii t -> v -> DynamicsProperties v ii t
setAngularVelocity dynamicsProperties newVelocity = dynamicsProperties { angularVelocity = newVelocity }
