{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Math.Transform
(
DeltaTransform(..)
, Translation(..)
, Rotation(..)
, Transform(..)
, PrincipalDirections(..)
, PerpProd(..)
)
where

import Data.Vect.Double.Base hiding(translation)

-- FIXME: rename this module with something more descriptive

class DeltaTransform p v | p -> v, v -> p where
  deltaTransform :: p -> v -> v

class Translation p v | p -> v, v -> p where
  translation  :: p -> v
  translate    :: v -> p -> p

class Rotation p r | p -> r, r -> p where
  -- rotation :: p -> r
  rotate   :: r -> p -> p

class (MultSemiGroup p, Matrix p, DeltaTransform p v, Rotation p a, Translation p v,
       PerpProd v a, DotProd v, DotProd a) =>
      Transform p v a | p -> v
                        , p -> a where
  transform :: p -> v -> v
  transform proj vect = deltaTransform proj vect &+ translation proj

class (Vector v, Vector a) => PerpProd v a | v -> a where
  perp :: v -> v -> a

class (Vector v) => PrincipalDirections v where
  principalDirections :: [ v ]
