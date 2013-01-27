{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.ShapeWithMargin
(
ShapeWithMargin(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Error
import Physics.Falling.Shape.ImplicitShape

newtype (DotProd v, ImplicitShape g v) => ShapeWithMargin g v = ShapeWithMargin g

instance (DotProd v, ImplicitShape g v) => ImplicitShape (ShapeWithMargin g v) v where
  supportPoint (ShapeWithMargin g) dir = (dir &* (margin / len dir)) &+ supportPoint g dir
