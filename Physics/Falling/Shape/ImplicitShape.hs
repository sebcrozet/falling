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

supportPointWithTransform :: (ImplicitShape  g v
                              , Transform      m v av
                              , UnitVector     v n)
                          => g -> n -> m -> m -> v
supportPointWithTransform g n t it = t `transform` supportPoint (it `deltaTransform` dir) g
                                     where
                                     dir = fromNormal n
