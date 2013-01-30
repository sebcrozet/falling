{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-} -- FIXME: a way to avoid that (for UnitVector instance)?

module Physics.Falling.Math.AnnotatedVector
(
AnnotatedVector(..)
, AnnotatedMatrix(..)
, vector
, annotation
)
where

import Physics.Falling.Math.Transform

data AnnotatedVector v a = AnnotatedVector v a

vector :: AnnotatedVector v a -> v
vector (AnnotatedVector v _) = v

annotation :: AnnotatedVector v a -> a
annotation (AnnotatedVector _ a) = a

-- fundeps will prevent instances of Translation m (AnnotatedVector v a)
--                               and Translation m v
-- at the same time.
-- So we wrap m on an AnnotatedMatrix newtype and declare the instance
-- Translation (AnnotatedMatrix m) (AnnotatedVector v a)
newtype AnnotatedMatrix m   = AnnotatedMatrix m

instance (Eq v) => Eq (AnnotatedVector v a) where
  AnnotatedVector v _ == AnnotatedVector v' _ = v == v'

instance (Vector v) => Vector (AnnotatedVector v a) where
  scalarMul s (AnnotatedVector v a) = AnnotatedVector (s *& v) a
  mapVec    f (AnnotatedVector v a) = AnnotatedVector (mapVec f v) a

instance (AbelianGroup v) => AbelianGroup (AnnotatedVector v a) where
  AnnotatedVector v _ &+ AnnotatedVector v' _ = AnnotatedVector (v &+ v') undefined
  AnnotatedVector v _ &- AnnotatedVector v' _ = AnnotatedVector (v &- v') undefined
  neg (AnnotatedVector v a)                   = AnnotatedVector (neg v)   a
  zero                                        = AnnotatedVector zero      undefined

instance (DotProd v) => DotProd (AnnotatedVector v a) where
  AnnotatedVector v _ &. AnnotatedVector v' _ = v &. v'

instance (UnitVector v n) =>
         UnitVector (AnnotatedVector v a) (AnnotatedVector n a) where
         mkNormal       (AnnotatedVector v a) = AnnotatedVector (mkNormal v)       a
         toNormalUnsafe (AnnotatedVector v a) = AnnotatedVector (toNormalUnsafe v) a 
         fromNormal     (AnnotatedVector n a) = AnnotatedVector (fromNormal n)     a

instance (DeltaTransform m v) => DeltaTransform (AnnotatedMatrix m) (AnnotatedVector v a) where
         (AnnotatedMatrix m) `deltaTransform` (AnnotatedVector v a) = AnnotatedVector (m `deltaTransform` v) a

instance (Translation m v) => Translation (AnnotatedMatrix m) (AnnotatedVector v a) where
         translation (AnnotatedMatrix m)                       = AnnotatedVector (translation  m) undefined
         translate   (AnnotatedVector v _) (AnnotatedMatrix m) = AnnotatedMatrix (translate v m)

instance (Transform m v, Vector v) => Transform (AnnotatedMatrix m) (AnnotatedVector v a)
