{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Collision.Detection.NarrowPhase
(
NarrowPhase(..)
, islandNarrowPhaseFilter
)
where

-- FIXME: this interface is not good if we want to implement an impulse cash
class NarrowPhase narrowPhaseType rigidBodyType contactManifoldType | narrowPhaseType -> contactManifoldType
                                                                      , narrowPhaseType -> rigidBodyType where
  update        :: (Int, rigidBodyType) -> (Int, rigidBodyType) -> narrowPhaseType -> narrowPhaseType
  collisions    :: narrowPhaseType -> contactManifoldType
  numCollisions :: narrowPhaseType -> Int

islandNarrowPhaseFilter :: NarrowPhase nf rb cm => (nf, Int) -> Bool
islandNarrowPhaseFilter (nf, _) = numCollisions nf /= 0
