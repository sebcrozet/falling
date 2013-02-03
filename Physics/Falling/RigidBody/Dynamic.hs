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
  getLinearVelocity                 :: d -> lv
  getAngularVelocity                :: d -> av
  getInverseMass                    :: d -> Double
  getWorldSpaceInverseInertiaTensor :: d -> ii
  getExternalLinearForce            :: d -> lv
  getExternalAngularForce           :: d -> av
  setExternalLinearForce            :: lv -> d -> d
  setExternalAngularForce           :: av -> d -> d
  setLinearVelocity                 :: lv -> d -> d
  setAngularVelocity                :: av -> d -> d

  -- functions with default implementations
  getVelocities                     :: d -> (lv, av)
  setVelocities                     :: (lv, av) -> d -> d
  applyCentralImpulse               :: lv -> d -> d
  applyAngularImpulse               :: av -> d -> d
  applyCentralPositionImpulse       :: lv -> d -> d
  applyAngularPositionImpulse       :: av -> d -> d
  applyImpulses                     :: lv -> av -> d -> d
  applyPositionImpulses             :: lv -> av -> d -> d

  getVelocities       d        = (getLinearVelocity d, getAngularVelocity d)
  setVelocities       (lv, av) = setAngularVelocity av . setLinearVelocity lv
  applyCentralImpulse lv d     = setLinearVelocity (getLinearVelocity d &+ centralImpulseDeltaVelotity d lv) d
  applyAngularImpulse av d     = setAngularVelocity (getAngularVelocity d &+ angularImpulseDeltaVelocity d av) d
  applyImpulses       lv av    = applyCentralImpulse lv . applyAngularImpulse av
  applyCentralPositionImpulse lv d  = translate (centralImpulseDeltaVelotity d lv) d
  applyAngularPositionImpulse av d  = rotate (angularImpulseDeltaVelocity d av) d
  applyPositionImpulses       lv av = applyCentralPositionImpulse lv . applyAngularPositionImpulse av


centralImpulseDeltaVelotity :: (Dynamic d t lv av ii) => d -> lv -> lv
centralImpulseDeltaVelotity d lv = lv &* getInverseMass d

angularImpulseDeltaVelocity :: (Dynamic d t lv av ii) => d -> av -> av
angularImpulseDeltaVelocity d av = getWorldSpaceInverseInertiaTensor d `applyToVector` av
