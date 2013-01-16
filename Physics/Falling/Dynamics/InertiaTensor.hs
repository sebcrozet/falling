{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Dynamics.InertiaTensor
(
InertiaTensor(..),
InverseInertiaTensor(..),
InertiaTensor3d,
InverseInertiaTensor3d
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Transform

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

newtype InertiaTensor3d        = InertiaTensor3d Proj4
newtype InverseInertiaTensor3d = InverseInertiaTensor3d Proj4

instance InertiaTensor InertiaTensor3d InverseInertiaTensor3d Vec3 Proj4 where
  inverseInertia (InertiaTensor3d inertia) = InverseInertiaTensor3d $ inverse inertia

instance InverseInertiaTensor InverseInertiaTensor3d Vec3 Proj4 where
  applyToVector (InverseInertiaTensor3d i) v         = i `transform` v
  toWorldSpaceTensor (InverseInertiaTensor3d i) t it = InverseInertiaTensor3d $ t .*. i .*. it
