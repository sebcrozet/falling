{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.ShapeReflection
(
ShapeReflection(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Shape.ImplicitShape

newtype (ImplicitShape g v) => ShapeReflection g v = ShapeReflection g

instance (ImplicitShape g v) => ImplicitShape (ShapeReflection g v) v where
  supportPoint (ShapeReflection g) dir = neg $ supportPoint g $ neg dir
