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

class DeltaTransform p v where
  deltaTransform          :: p -> v -> v
  deltaTransformTranspose :: p -> v -> v

-- | Class of objects holding translation informations.
class Translatable p v | p -> v where
  -- | The translation of the object.
  translation  :: p -> v

  -- | Appends a translation to the object.
  translate    :: v -> p -> p

-- | Class of objects holding rotation informations.
class Rotatable p r | p -> r where
  -- | The rotation of the object.
  rotation :: p -> r

  -- | Appends a rotation to the object.
  rotate   :: r -> p -> p

-- | Appends a rotation to an object. The rotation is applied relative to a use-defined point
-- instead of the origin.
rotateWrtPoint :: (AbelianGroup v, Rotatable p r, Translatable p v) => r -> v -> p -> p
rotateWrtPoint ang point = translate point . rotate ang . translate (neg point)

-- | Appends a rotation to an object. The rotation is applied relative to object center.
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
