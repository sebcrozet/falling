{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.CollisionVolume
(
CollisionVolume(..)
)
where

class CollisionVolume a v | a -> v where
  getCollisionVolume :: a -> v
  setCollisionVolume :: v -> a -> a
