{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.ShapeReflection
(
ShapeReflection(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape

newtype (ImplicitShape g v) => ShapeReflection g v = ShapeReflection g
                                                     deriving(Show)

instance (ImplicitShape g v) => ImplicitShape (ShapeReflection g v) v where
  supportPoint (ShapeReflection g) dir = neg $ supportPoint g $ neg dir
