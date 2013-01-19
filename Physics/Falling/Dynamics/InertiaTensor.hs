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
       InertiaTensor i ii v t | v -> i,  i -> v,
                                t -> i,  i -> t,
                                ii -> i, i -> ii  where
  inverseInertia      :: i -> ii
  parallelAxisTheorem :: Double -> i -> v -> v -> i

  parallelAxisTheorem = undefined -- FIXME

class (Vector v) => InverseInertiaTensor ii v t | v -> ii, -- FIXME: no restriction on t?
                                                  ii -> v,
                                                  t -> ii,
                                                  ii -> t where
  toWorldSpaceTensor :: ii -> t -> t -> ii
  applyToVector      :: ii -> v -> v

