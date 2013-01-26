{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.TransformedShape
(
TransformedShape(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape

data (ImplicitShape g v, Transform t v) => TransformedShape g t v = TransformedShape g t t

instance (ImplicitShape g v, Transform t v) => ImplicitShape (TransformedShape g t v) v where
  supportPoint (TransformedShape g t it) dir = supportPointWithTransform g dir t it
