{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.Positionable
(
Positionable(..)
)
where

import Data.Vect.Double.Base
import qualified Physics.Falling.Math.Transform as T

class (T.Transform t v av) => Positionable p t v av | p -> t where
  getLocalToWorld       :: p -> t
  getWorldToLocal       :: p -> t
  setTransforms         :: t -> t -> p -> p

  -- functions with default implementations
  setLocalToWorld       :: t -> p -> p
  setWorldToLocal       :: t -> p -> p
  resetToIdentity       :: p -> p
  prependTransform      :: t -> p -> p
  appendTransform       :: t -> p -> p
  translate             :: v -> p -> p
  rotate                :: av -> p -> p

  setLocalToWorld t = setTransforms t (inverse t)
  setWorldToLocal t = setTransforms (inverse t) t
  resetToIdentity      = setTransforms idmtx idmtx
  prependTransform t p = setLocalToWorld (t .*. getLocalToWorld p) p
  appendTransform  t p = setLocalToWorld (getLocalToWorld p .*. t) p
  translate        t p = setLocalToWorld (T.translate t $ getLocalToWorld p) p
  rotate           w p = setLocalToWorld (T.rotate w $ getLocalToWorld p) p
