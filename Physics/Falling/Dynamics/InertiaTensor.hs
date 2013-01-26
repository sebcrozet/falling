{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Dynamics.InertiaTensor
(
InertiaTensor(..)
, InverseInertiaTensor(..)
)
where

import Data.Vect.Double.Base

class (Vector v, InverseInertiaTensor ii v t) =>
       InertiaTensor i ii v t | t -> i, i -> v, i -> t, i -> ii  where
  inverseInertia      :: i -> ii
  parallelAxisTheorem :: Double -> i -> v -> v -> i

  parallelAxisTheorem = undefined -- FIXME

class (Vector v) => InverseInertiaTensor ii v t | ii -> v, ii -> t where
  toWorldSpaceTensor :: ii -> t -> t -> ii
  applyToVector      :: ii -> v -> v

