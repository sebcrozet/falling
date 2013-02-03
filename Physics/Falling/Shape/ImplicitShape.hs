{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Shape.ImplicitShape
(
ImplicitShape(..)
, supportPointWithTransform
)
where

import Physics.Falling.Math.Transform

class (Vector v) => ImplicitShape g v | g -> v where
  supportPoint :: g -> v -> v

supportPointWithTransform :: (ImplicitShape  g v
                              , Transform    m v)
                          => g -> v -> m -> v
supportPointWithTransform g dir t = t `transform` supportPoint g (t `deltaTransformTranspose` dir)
