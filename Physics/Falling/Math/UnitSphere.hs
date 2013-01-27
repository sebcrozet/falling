module Physics.Falling.Math.UnitSphere
(
UnitSphere(..)
)
where

class UnitSphere v where
  unitSphereSamples  :: [ v ]
  nUnitSphereSamples :: Int -> [ v ]
  nUnitSphereSamples n = take n unitSphereSamples
