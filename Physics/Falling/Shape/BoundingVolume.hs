module Physics.Falling.Shape.BoundingVolume
(
BoundingVolume(..)
, LoozeBoundingVolume(..)
)
where

class BoundingVolume bv where
  merge      :: bv -> bv -> bv
  intersects :: bv -> bv -> Bool
  contains   :: bv -> bv -> Bool

class BoundingVolume bv => LoozeBoundingVolume bv where
  loozen :: Double -> bv -> bv
