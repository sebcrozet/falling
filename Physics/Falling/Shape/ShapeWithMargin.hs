{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.ShapeWithMargin
(
ShapeWithMargin(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Math.Error
import Physics.Falling.Shape.ImplicitShape

newtype (DotProd v, ImplicitShape g v) => ShapeWithMargin g v = ShapeWithMargin g
                                                                deriving(Show)

instance (DotProd v, ImplicitShape g v) => ImplicitShape (ShapeWithMargin g v) v where
  supportPoint (ShapeWithMargin g) dir = (dir &* (margin / len dir)) &+ supportPoint g dir
