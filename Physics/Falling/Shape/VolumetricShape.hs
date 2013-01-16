{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Shape.VolumetricShape
(
VolumetricShape(..)
)
where

import Physics.Falling.Dynamics.InertiaTensor

class (InertiaTensor i ii av t, InverseInertiaTensor ii av t) =>
      VolumetricShape s i ii av t | s -> i where
  volume                          :: s -> Double
  objectFrameInertiaTensor        :: s -> Double -> i

  -- allow some simple shapes to give an optimized inverse tensor computation
  objectFrameInverseInertiaTensor :: s -> Double -> ii
  objectFrameInverseInertiaTensor s mass = inverseInertia $ objectFrameInertiaTensor s mass
