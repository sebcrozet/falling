{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Shape.ImplicitShape
(
ImplicitShape(..)
, supportPointWithTransform
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Transform

class (Vector v) => ImplicitShape g v | g -> v where
  supportPoint :: g -> v -> v

supportPointWithTransform :: (ImplicitShape  g v
                              , Transform    m v)
                          => g -> v -> m -> m -> v
supportPointWithTransform g dir t it = t `transform` supportPoint g (it `deltaTransform` dir)
