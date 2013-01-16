{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Dynamic
(
Dynamic(..)
)
where

import Data.Vect.Double.Base
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

  getVelocities d          = (getLinearVelocity d, getAngularVelocity d)
  setVelocities (lv, av)   = setAngularVelocity av . setLinearVelocity lv
  applyCentralImpulse lv d = setLinearVelocity (getLinearVelocity d &+ centralImpulseDeltaVelotity lv d) d
  applyAngularImpulse av d = setAngularVelocity (getAngularVelocity d &+ angularImpulseDeltaVelocity av d) d
  applyImpulses    lv av   = applyCentralImpulse lv . applyAngularImpulse av
  applyCentralPositionImpulse lv d  = translate (centralImpulseDeltaVelotity lv d) d
  applyAngularPositionImpulse av d  = rotate (angularImpulseDeltaVelocity av d) d
  applyPositionImpulses       lv av = applyCentralPositionImpulse lv . applyAngularPositionImpulse av


centralImpulseDeltaVelotity :: (Dynamic d t lv av ii) => lv -> d -> lv
centralImpulseDeltaVelotity lv d = lv &* getInverseMass d

angularImpulseDeltaVelocity :: (Dynamic d t lv av ii) => av -> d -> av
angularImpulseDeltaVelocity av d = getWorldSpaceInverseInertiaTensor d `applyToVector` av