{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Shape.ImplicitShape
(
ImplicitShape(..)
, supportPointWithTransform
, sampleCSO
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Transform

class (Vector v) => ImplicitShape g v where
  supportPoint :: v -> g -> v

supportPointWithTransform :: (ImplicitShape  g v
                              , Transform    m v
                              , UnitVector   v n)
                          => g -> n -> m -> m -> v
supportPointWithTransform g n t it = t `transform` supportPoint (it `deltaTransform` dir) g
                                     where
                                     dir = fromNormal n

sampleCSO :: (ImplicitShape g1 v, ImplicitShape g2 v, Transform m v, UnitVector v n) =>
             (g1, m, m) -> (g2, m, m) -> n -> v
sampleCSO (g1, t1, it1) (g2, t2, it2) dir = p1 &- p2
                                            where
                                            p1     = supportPointWithTransform g1 dir    t1 it1
                                            p2     = supportPointWithTransform g2 oppDir t2 it2
                                            oppDir = toNormalUnsafe $ neg $ fromNormal dir
