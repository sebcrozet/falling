{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Dynamic
(
Dynamic(..)
, centralImpulseDeltaVelotity
, angularImpulseDeltaVelocity
)
where

import Physics.Falling.Math.Transform hiding(translate, rotate)
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.Dynamics.InertiaTensor

-- | Class of objects having dynamic physics state properties. This includes linear and angular
-- velocities, mass, and inertia tensor. Also provides functions to modify those properties.
--
-- Minimum complete implementation:
--
--   * 'linearVelocity'
--
--   * 'angularVelocity'               
--
--   * 'inverseMass'
--
--   * 'worldSpaceInverseInertiaTensor'
--
--   * 'externalLinearForce'
--
--   * 'externalAngularForce'
--
--   * 'setExternalLinearForce'
--
--   * 'setExternalAngularForce'
--
--   * 'setLinearVelocity'
--
--   * 'setAngularVelocity'
class (Positionable d t lv av, InverseInertiaTensor ii av t) =>
      Dynamic d t lv av ii | d -> lv
                             , d -> av
                             , d -> ii
                             where
  -- | The linear velocity of the dynamic object.
  linearVelocity                 :: d -> lv

  -- | The angular velocity of the dynamic object.
  angularVelocity                :: d -> av

  -- | The inverse mass of the dynamic object. The smaller the inverse mass is, the greater
  -- resistance to changes of linear velocity the dynamic object will deliver.
  inverseMass                    :: d -> Double

  -- | The inverse inertia tensor in world space coordinates of the dynamic object. The
  -- smaller the inverse inertia tensor is, the greater resistance to changes of angular velocity
  -- the dynamic object will deliver.
  worldSpaceInverseInertiaTensor :: d -> ii

  -- | The total external force applied on the object center of mass (like the gravity).
  externalLinearForce            :: d -> lv

  -- | The total extrenal torque affecting the object.
  externalAngularForce           :: d -> av

  -- | Sets the total external force applied on the object center of mass (like the gravity).
  -- External forces must be set to each body individually.
  setExternalLinearForce         :: lv -> d -> d

  -- | Sets the total extrenal torque affecting the object.
  -- External forces must be set to each body individually.
  setExternalAngularForce        :: av -> d -> d

  -- | Sets the linear velocity of the dynamic object.
  setLinearVelocity              :: lv -> d -> d

  -- | Sets the angular velocity of the dynamic object.
  setAngularVelocity             :: av -> d -> d

  -- functions with default implementations
  --
  -- | Both (linear and angular) velocities of the dynamic object.
  velocities                     :: d -> (lv, av)

  -- | Sets both (linear and angular) velocities of the dynamic object.
  setVelocities                  :: (lv, av) -> d -> d

  -- | Applies an impulse on the object center of mass, affecting its linear velocity. The change
  -- of linear velocity equals the object inverse mass times the impulse.
  applyCentralImpulse            :: lv -> d -> d

  -- | Applies an impulse affecting the dynamic object angular velocity. The change of angular
  -- velocity equals the object inverse inertia tensor times the impulse.
  applyAngularImpulse            :: av -> d -> d

  -- | Applies an impulse on the object center of mass, affecting its position. The change of
  -- position equals the object inverse mass times the impulse.
  applyCentralPositionImpulse    :: lv -> d -> d

  -- | Applies an impulse affecting the dynamic object orientation. The change of orientation
  -- equals the object inverse inertia tensor times the impulse.
  applyAngularPositionImpulse    :: av -> d -> d

  -- | Applies an angular and a linear impulse on the object at the same time.
  applyImpulses                  :: lv -> av -> d -> d

  -- | Applies an angular and a linear positional impulse on the object at the same time.
  applyPositionImpulses          :: lv -> av -> d -> d

  velocities          d        = (linearVelocity d, angularVelocity d)
  setVelocities       (lv, av) = setAngularVelocity av . setLinearVelocity lv
  applyCentralImpulse lv d     = setLinearVelocity (linearVelocity d &+ centralImpulseDeltaVelotity d lv) d
  applyAngularImpulse av d     = setAngularVelocity (angularVelocity d &+ angularImpulseDeltaVelocity d av) d
  applyImpulses       lv av    = applyCentralImpulse lv . applyAngularImpulse av
  applyCentralPositionImpulse lv d  = translate (centralImpulseDeltaVelotity d lv) d
  applyAngularPositionImpulse av d  = rotate (angularImpulseDeltaVelocity d av) d
  applyPositionImpulses       lv av = applyCentralPositionImpulse lv . applyAngularPositionImpulse av


-- | Computes the change of linear velocity produced by a central impulse on a given dynamic
-- object. Can also be used to compute the change of position due to a central positional impulse.
centralImpulseDeltaVelotity :: (Dynamic d t lv av ii) => d -> lv -> lv
centralImpulseDeltaVelotity d lv = lv &* inverseMass d

-- | Computes the change of angular velocity produced by a central impulse on a given dynamic
-- object. Can also be used to compute the change of orientation due to an angular positional
-- impluse.
angularImpulseDeltaVelocity :: (Dynamic d t lv av ii) => d -> av -> av
angularImpulseDeltaVelocity d av = worldSpaceInverseInertiaTensor d `applyToVector` av
