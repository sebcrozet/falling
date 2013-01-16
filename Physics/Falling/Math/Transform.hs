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

import Data.Vect.Double.Util.Projective
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



instance DeltaTransform Proj4 Vec3 where
  deltaTransform p v = dt *. v
                       where
                       t  = fromProjective p
                       dt = trim t :: Mat3

instance Translation Proj4 Vec3 where
  translation p = trim t :: Vec3
                  where
                  (Mat4 _ _ _ t) = fromProjective p
  translate     = translate4

instance Rotation Proj4 Vec3 where
  rotate orientationVector = rotateProj4 magnitude normal
                             where
                             magnitude = len orientationVector
                             normal    = toNormalUnsafe $ orientationVector &* magnitude

instance PerpProd Vec3 Vec3 where
  perp = crossprod

instance Transform Proj4 Vec3 Vec3

instance PrincipalDirections Vec3 where
  principalDirections = [ Vec3 1 0 0, Vec3 0 1 0 , Vec3 0 0 1,
                          Vec3 (-1) 0 0, Vec3 0 (-1) 0, Vec3 0 0 (-1) ]

instance PrincipalDirections Vec4 where
  principalDirections = [ Vec4 1 0 0 0 , Vec4 0 1 0 0 ,
                          Vec4 0 0 1 0, Vec4 0 0 0 1,
                          Vec4 (-1) 0 0 0, Vec4 0 (-1) 0 0,
                          Vec4 0 0 (-1) 0, Vec4 0 0 0 (-1) ]
