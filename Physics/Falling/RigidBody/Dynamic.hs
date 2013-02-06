{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Dynamic
(
Dynamic(..)
)
where

import Physics.Falling.Math.Transform hiding(translate, rotate)
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.Dynamics.InertiaTensor

class (Positionable d t lv av, InverseInertiaTensor ii av t) =>
      Dynamic d t lv av ii | d -> lv
                             , d -> av
                             , d -> ii
                             where
  linearVelocity                 :: d -> lv
  angularVelocity                :: d -> av
  inverseMass                    :: d -> Double
  worldSpaceInverseInertiaTensor :: d -> ii
  externalLinearForce            :: d -> lv
  externalAngularForce           :: d -> av
  setExternalLinearForce            :: lv -> d -> d
  setExternalAngularForce           :: av -> d -> d
  setLinearVelocity                 :: lv -> d -> d
  setAngularVelocity                :: av -> d -> d

  -- functions with default implementations
  velocities                     :: d -> (lv, av)
  setVelocities                     :: (lv, av) -> d -> d
  applyCentralImpulse               :: lv -> d -> d
  applyAngularImpulse               :: av -> d -> d
  applyCentralPositionImpulse       :: lv -> d -> d
  applyAngularPositionImpulse       :: av -> d -> d
  applyImpulses                     :: lv -> av -> d -> d
  applyPositionImpulses             :: lv -> av -> d -> d

  velocities       d        = (linearVelocity d, angularVelocity d)
  setVelocities       (lv, av) = setAngularVelocity av . setLinearVelocity lv
  applyCentralImpulse lv d     = setLinearVelocity (linearVelocity d &+ centralImpulseDeltaVelotity d lv) d
  applyAngularImpulse av d     = setAngularVelocity (angularVelocity d &+ angularImpulseDeltaVelocity d av) d
  applyImpulses       lv av    = applyCentralImpulse lv . applyAngularImpulse av
  applyCentralPositionImpulse lv d  = translate (centralImpulseDeltaVelotity d lv) d
  applyAngularPositionImpulse av d  = rotate (angularImpulseDeltaVelocity d av) d
  applyPositionImpulses       lv av = applyCentralPositionImpulse lv . applyAngularPositionImpulse av


centralImpulseDeltaVelotity :: (Dynamic d t lv av ii) => d -> lv -> lv
centralImpulseDeltaVelotity d lv = lv &* inverseMass d

angularImpulseDeltaVelocity :: (Dynamic d t lv av ii) => d -> av -> av
angularImpulseDeltaVelocity d av = worldSpaceInverseInertiaTensor d `applyToVector` av
