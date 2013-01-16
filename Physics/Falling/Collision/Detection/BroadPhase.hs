{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Collision.Detection.BroadPhase
(
BroadPhase(..)
)
where

class BroadPhase bf rb where
  addBody    :: rb -> bf -> bf
  removeBody :: rb -> bf -> bf
  update     :: [ rb ] -> bf -> ([ (rb, rb) ], [ (rb, rb) ], bf)
