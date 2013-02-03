module Physics.Falling.Shape.CSO
(
CSO
, AnnotatedCSO
, mkCSO
, mkCSOWithTransforms
, mkAnnotatedCSO
, mkAnnotatedCSOWithTransforms
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.TransformedShape
import Physics.Falling.Shape.MinkowskiSum
import Physics.Falling.Shape.ShapeReflection

type CSO          g1 g2 v = MinkowskiSum g1 (ShapeReflection g2 v) v
type AnnotatedCSO g1 g2 v = AnnotatedMinkowskiSum g1 (ShapeReflection g2 v) v

mkCSO :: (ImplicitShape g1 v, ImplicitShape g2 v) => g1 -> g2 -> CSO g1 g2 v
mkCSO g1 g2 = MinkowskiSum g1 (ShapeReflection g2)

mkCSOWithTransforms :: (ImplicitShape g1 v,  ImplicitShape g2 v, Transform m v) =>
                       (g1, m) -> (g2, m) -> CSO (TransformedShape g1 m v) (TransformedShape g2 m v) v
mkCSOWithTransforms (g1, t1) (g2, t2) =
                    MinkowskiSum (TransformedShape g1 t1) $ ShapeReflection $ TransformedShape g2 t2

mkAnnotatedCSO :: (ImplicitShape g1 v, ImplicitShape g2 v) => g1 -> g2 -> AnnotatedCSO g1 g2 v
mkAnnotatedCSO g1 g2 = AnnotatedMinkowskiSum g1 (ShapeReflection g2)

mkAnnotatedCSOWithTransforms :: (ImplicitShape g1 v,  ImplicitShape g2 v, Transform m v) =>
                       (g1, m) -> (g2, m) -> AnnotatedCSO (TransformedShape g1 m v) (TransformedShape g2 m v) v
mkAnnotatedCSOWithTransforms (g1, t1) (g2, t2) =
                    AnnotatedMinkowskiSum (TransformedShape g1 t1) $ ShapeReflection $ TransformedShape g2 t2
