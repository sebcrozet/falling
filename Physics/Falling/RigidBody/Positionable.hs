{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Positionable
(
Positionable(..)
)
where

import Physics.Falling.Math.Transform hiding(translate)
import qualified Physics.Falling.Math.Transform as T

class (T.TransformSystem t v av) => Positionable p t v av | p -> t where
  localToWorld          :: p -> t
  worldToLocal          :: p -> t
  setTransforms         :: t -> t -> p -> p

  -- functions with default implementations
  setLocalToWorld       :: t -> p -> p
  setWorldToLocal       :: t -> p -> p
  resetToIdentity       :: p -> p
  prependTransform      :: t -> p -> p
  appendTransform       :: t -> p -> p
  translate             :: v -> p -> p
  rotate                :: av -> p -> p

  setLocalToWorld t    = setTransforms t (inverse t)
  setWorldToLocal t    = setTransforms (inverse t) t
  resetToIdentity      = setTransforms idmtx idmtx
  prependTransform t p = setLocalToWorld (t .*. localToWorld p) p
  appendTransform  t p = setLocalToWorld (localToWorld p .*. t) p
  translate        t p = setLocalToWorld (T.translate t $ localToWorld p) p
  rotate           w p = setLocalToWorld (T.rotateWrtCenter w $ localToWorld p) p
