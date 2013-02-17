{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Shape.Box
(
Box(..)
-- , boxVolume
)
where

-- import qualified Data.Foldable as F

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.ImplicitShape

newtype (Pointwise v, Vector v) => Box v = Box v

instance (Pointwise v, Vector v) => ImplicitShape (Box v) v where
   supportPoint (Box v) support = mapVec signum support &! v

-- boxVolume :: (F.Foldable v, Pointwise v, Vector v) => Box v -> Double
-- boxVolume (Box v) = F.foldr (\a b -> b * a * 2.0) 0 v
