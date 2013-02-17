{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Shape.TransformableShape
(
TransformableShape(..)
)
where

class TransformableShape s t s' | s -> s' where
  transformShape :: t -> s -> s'
