{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.RigidBody.CollisionVolume
(
CollisionVolume(..)
, WorldSpaceCollisionVolume(..)
)
where

class CollisionVolume a cv | a -> cv where
  collisionVolume    :: a -> cv
  setCollisionVolume :: cv -> a -> a

class WorldSpaceCollisionVolume a cv | a -> cv where
  worldSpaceCollisionVolume :: a -> cv
