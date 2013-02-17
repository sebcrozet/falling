{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Shape.HasBoundingVolume
(
HasBoundingVolume(..)
)
where

import Physics.Falling.Shape.BoundingVolume

class BoundingVolume bv => HasBoundingVolume g bv | g -> bv where
  boundingVolume            :: g -> bv
