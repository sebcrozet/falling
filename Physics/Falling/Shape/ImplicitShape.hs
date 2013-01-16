{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Shape.ImplicitShape
(
ImplicitShape(..)
, supportPointWithTransform
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Transform

class (Vector v) => ImplicitShape g v where
  supportPoint :: v -> g -> v

supportPointWithTransform :: (ImplicitShape  g v,
                              LeftModule     m v,
                              DeltaTransform m v,
                              UnitVector     v n)
                          => g -> n -> m -> m -> v
supportPointWithTransform g n t it = t *. supportPoint (it `deltaTransform` dir) g
                                     where
                                     dir = fromNormal n
