{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Positionable
(
Positionable(..)
)
where

import Physics.Falling.Math.Transform hiding(translate)
import qualified Physics.Falling.Math.Transform as T

-- | Class of objects having a position and an orientation of the physic world.
--
-- Minimum complete implementation:
--
--   * 'localToWorld'
--
--   * 'worldToLocal'
--
--   * 'setTransforms'
class (T.TransformSystem t v av) => Positionable p t v av | p -> t where
  -- | The object transform transforming a vector from the object local coordinate system to the
  -- world coordinate system.
  localToWorld          :: p -> t

  -- | The object transform transforming a vector from the world coordinate system to the object
  -- local coordinate system.
  worldToLocal          :: p -> t

  -- | Sets the 'localToWorld' and 'worldToLocal' transforms of the object. This method should not
  -- be called directly. Use 'setLocalToWorld' or 'setWorldToLocal' instead.
  setTransforms         :: t -> t -> p -> p

  -- functions with default implementations
  --
  -- | Sets the transform correponding to the object position and orientation.
  setLocalToWorld       :: t -> p -> p

  -- | Sets the transform transforming a vector from the world coordinate system to the object
  -- local coordinate system.
  setWorldToLocal       :: t -> p -> p

  -- | Resets the position and orientation of the object such that it is centered at the origin and
  -- has no rotations.
  resetToIdentity       :: p -> p

  -- | Prepends a transformation to the object. Useful to apply a rotation on the object local
  -- space (e.g. before any translation). It is assumed that such transformation does not contain
  -- scaling.
  prependTransform      :: t -> p -> p

  -- | Appends a transformation to the object. Useful to apply a translation after any rotation of
  -- the object. It is assumed that such transformation does not contain scaling.
  appendTransform       :: t -> p -> p

  -- | Translates an object.
  translate             :: v -> p -> p

  -- | Rotates an object. The rotation is applied before any other transformations (e.g. before any
  -- translation).
  rotate                :: av -> p -> p

  setLocalToWorld t    = setTransforms t (inverse t)
  setWorldToLocal t    = setTransforms (inverse t) t
  resetToIdentity      = setTransforms idmtx idmtx
  prependTransform t p = setLocalToWorld (t .*. localToWorld p) p
  appendTransform  t p = setLocalToWorld (localToWorld p .*. t) p
  translate        t p = setLocalToWorld (T.translate t $ localToWorld p) p
  rotate           w p = setLocalToWorld (T.rotateWrtCenter w $ localToWorld p) p
