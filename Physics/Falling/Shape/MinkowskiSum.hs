{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.MinkowskiSum
(
MinkowskiSum(..)
, AnnotatedMinkowskiSum(..)
)
where

import Data.Vect.Double.Base
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Math.AnnotatedVector

data (ImplicitShape g1 v, ImplicitShape g2 v) => MinkowskiSum          g1 g2 v = MinkowskiSum g1 g2
data (ImplicitShape g1 v, ImplicitShape g2 v) => AnnotatedMinkowskiSum g1 g2 v = AnnotatedMinkowskiSum g1 g2

instance (ImplicitShape g1 v, ImplicitShape g2 v) => ImplicitShape (MinkowskiSum g1 g2 v) v where
  supportPoint (MinkowskiSum g1 g2) dir = supportPoint g1 dir &+ supportPoint g2 dir

instance (ImplicitShape g1 v, ImplicitShape g2 v) =>
         ImplicitShape (AnnotatedMinkowskiSum g1 g2 v) (AnnotatedVector v (v, v)) where
  supportPoint (AnnotatedMinkowskiSum g1 g2) dir = AnnotatedVector (sp1 &+ sp2) (sp1, sp2)
                                                   where
                                                   sp1 = supportPoint g1 $ vector dir
                                                   sp2 = supportPoint g2 $ vector dir
