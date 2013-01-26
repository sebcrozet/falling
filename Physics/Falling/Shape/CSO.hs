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
import Physics.Falling.Shape.MinkovskiSum
import Physics.Falling.Shape.ShapeReflection

type CSO          g1 g2 v = MinkovskiSum g1 (ShapeReflection g2 v) v
type AnnotatedCSO g1 g2 v = AnnotatedMinkovskiSum g1 (ShapeReflection g2 v) v

mkCSO :: (ImplicitShape g1 v, ImplicitShape g2 v) => g1 -> g2 -> CSO g1 g2 v
mkCSO g1 g2 = MinkovskiSum g1 (ShapeReflection g2)

mkCSOWithTransforms :: (ImplicitShape g1 v,  ImplicitShape g2 v, Transform m v) =>
                       (g1, m, m) -> (g2, m, m) -> CSO (TransformedShape g1 m v) (TransformedShape g2 m v) v
mkCSOWithTransforms (g1, t1, it1) (g2, t2, it2) =
                    MinkovskiSum (TransformedShape g1 t1 it1) $ ShapeReflection $ TransformedShape g2 t2 it2

mkAnnotatedCSO :: (ImplicitShape g1 v, ImplicitShape g2 v) => g1 -> g2 -> AnnotatedCSO g1 g2 v
mkAnnotatedCSO g1 g2 = AnnotatedMinkovskiSum g1 (ShapeReflection g2)

mkAnnotatedCSOWithTransforms :: (ImplicitShape g1 v,  ImplicitShape g2 v, Transform m v) =>
                       (g1, m, m) -> (g2, m, m) -> AnnotatedCSO (TransformedShape g1 m v) (TransformedShape g2 m v) v
mkAnnotatedCSOWithTransforms (g1, t1, it1) (g2, t2, it2) =
                    AnnotatedMinkovskiSum (TransformedShape g1 t1 it1) $ ShapeReflection $ TransformedShape g2 t2 it2
