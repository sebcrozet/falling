{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Math.Transform
(
module Data.Vect.Double.Base
, DeltaTransform(..)
, Translation(..)
, Rotation(..)
, Transform(..)
, TransformSystem
, PerpProd(..)
)
where

import Data.Vect.Double.Base hiding(translation)

-- FIXME: rename this module with something more descriptive

class DeltaTransform p v where
  deltaTransform :: p -> v -> v

class Translation p v | p -> v where
  translation  :: p -> v
  translate    :: v -> p -> p

class Rotation p r | p -> r where
  -- rotation :: p -> r
  rotate   :: r -> p -> p

class (DeltaTransform p v, Vector v, Translation p v) => Transform p v where
  transform :: p -> v -> v
  transform proj vect = deltaTransform proj vect &+ translation proj

class (Vector v, Vector a) => PerpProd v a | v -> a where
  perp :: v -> v -> a

class (DeltaTransform p v
       , Rotation       p a
       , Transform      p v
       , PerpProd       v a
       , MultSemiGroup  p
       , Matrix         p
       , DotProd        v
       , DotProd        a) =>
      TransformSystem p v a
