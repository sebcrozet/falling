{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Math.Transform
(
module Data.Vect.Double.Base
, DeltaTransform(..)
, Translatable(..)
, Rotatable(..)
, Transform(..)
, TransformSystem
, PerpProd(..)
, rotateWrtPoint
, rotateWrtCenter
)
where

import Data.Vect.Double.Base hiding(translation)

-- FIXME: rename this module with something more descriptive

class DeltaTransform p v where
  deltaTransform          :: p -> v -> v
  deltaTransformTranspose :: p -> v -> v

class Translatable p v | p -> v where
  translation  :: p -> v
  translate    :: v -> p -> p

class Rotatable p r | p -> r where
  rotation :: p -> r
  rotate   :: r -> p -> p

rotateWrtPoint :: (AbelianGroup v, Rotatable p r, Translatable p v) => r -> v -> p -> p
rotateWrtPoint ang point = translate point . rotate ang . translate (neg point)

rotateWrtCenter :: (AbelianGroup v, Rotatable p r, Translatable p v) => r -> p -> p
rotateWrtCenter ang p = rotateWrtPoint ang (translation p) p


class (DeltaTransform p v, Vector v, Translatable p v) => Transform p v where
  transform :: p -> v -> v
  transform proj vect = deltaTransform proj vect &+ translation proj

class (Vector v, Vector a) => PerpProd v a | v -> a where
  perp :: v -> v -> a

class (DeltaTransform   p v
       , Rotatable      p a
       , Transform      p v
       , PerpProd       v a
       , MultSemiGroup  p
       , Matrix         p
       , DotProd        v
       , DotProd        a) =>
  TransformSystem p v a
