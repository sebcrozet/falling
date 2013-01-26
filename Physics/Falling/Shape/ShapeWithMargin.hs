{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.ShapeWithMargin
(
margin
, ShapeWithMargin(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Shape.ImplicitShape

margin :: Double
margin = 0.04

newtype (DotProd v, ImplicitShape g v) => ShapeWithMargin g v = ShapeWithMargin g

instance (DotProd v, ImplicitShape g v) => ImplicitShape (ShapeWithMargin g v) v where
  supportPoint (ShapeWithMargin g) dir = (dir &* (margin / len dir)) &+ supportPoint g dir
