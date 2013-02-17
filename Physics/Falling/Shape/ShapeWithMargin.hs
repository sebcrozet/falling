{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Physics.Falling.Shape.ShapeWithMargin
(
ShapeWithMargin(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape

data ShapeWithMargin g = ShapeWithMargin g Double
                         deriving(Show)

instance (DotProd v, ImplicitShape g v) => ImplicitShape (ShapeWithMargin g) v where
  supportPoint (ShapeWithMargin g margin) dir = (dir &* (margin / len dir)) &+ supportPoint g dir
