{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-} -- FIXME: a way to avoid that (for UnitVector instance)?

module Physics.Falling.Math.AnnotatedVector
(
AnnotatedVector(..)
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

instance (Dimension v) => Dimension (AnnotatedVector v a) where
  dim (AnnotatedVector v _) = dim v

instance (UnitVector v n) =>
         UnitVector (AnnotatedVector v a) (AnnotatedVector n a) where
         mkNormal       (AnnotatedVector v a) = AnnotatedVector (mkNormal v)       a
         toNormalUnsafe (AnnotatedVector v a) = AnnotatedVector (toNormalUnsafe v) a 
         fromNormal     (AnnotatedVector n a) = AnnotatedVector (fromNormal n)     a
