{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Physics.Falling.Shape.TransformedShape
(
TransformedShape(..)
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.TransformableShape
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.VolumetricShape

data (Transform t v) => TransformedShape g t v = TransformedShape g t
                                                 deriving(Show)

instance (ImplicitShape g v, Transform t v) => ImplicitShape (TransformedShape g t v) v where
  supportPoint (TransformedShape g t) dir = supportPointWithTransform g dir t

instance (Transform t v, MultSemiGroup t, TransformableShape g t g') =>
         TransformableShape (TransformedShape g t v) t g' where
  transformShape t' (TransformedShape g t) = transformShape (t .*. t') g

instance (Transform t v) => Translatable (TransformedShape g t v) v where
  translation (TransformedShape _ t)                        = translation t
  translate   v                      (TransformedShape s t) = TransformedShape s (translate v t)

instance (Rotatable t av, Transform t v) => Rotatable (TransformedShape g t v) av where
  rotation (TransformedShape _ t)                        = rotation t
  rotate   av                     (TransformedShape s t) = TransformedShape s (rotate av t)

instance (Transform t v, VolumetricShape g i ii av t) =>
         VolumetricShape (TransformedShape g t v) i ii av t where
  volume                   (TransformedShape g _)   = volume g -- FIXME: the transform plays a role
  objectFrameInertiaTensor (TransformedShape g _) m = objectFrameInertiaTensor g m -- FIXME: the transform plays a role


