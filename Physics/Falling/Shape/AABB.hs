module Physics.Falling.Shape.AABB
(
AABB(InfiniteAABB)
, aabbFromBounds
, aabbFromBoundsUnsafe
)
where

import Data.List

import Physics.Falling.Shape.BoundingVolume

-- FIXME: this definition is too permissive since there is no type-level restictions on the
-- dimension
data AABB = AABB [ Double ] [ Double ] -- mins and maxs
          | InfiniteAABB

instance BoundingVolume AABB where
  merge (AABB mn1 mx1) (AABB mn2 mx2) = AABB (zipWith min mn1 mn2) (zipWith max mx1 mx2)
  merge InfiniteAABB   _              = InfiniteAABB
  merge _              InfiniteAABB   = InfiniteAABB

  intersects (AABB mn1 mx1) (AABB mn2 mx2) = not $ any dontIntersects $ zip4 mn1 mx1 mn2 mx2
                                             where
                                             dontIntersects (min1, max1, min2, max2) = min1 > max2
                                                                                       || min2 > max1
                                                                                       || max1 < min2
                                                                                       || max2 < min1
  intersects InfiniteAABB   _              = True
  intersects _              InfiniteAABB   = True

  contains InfiniteAABB   _              = True
  contains _              InfiniteAABB   = False
  contains (AABB mn1 mx1) (AABB mn2 mx2) = all contain1d $ zip4 mn1 mx1 mn2 mx2
                                           where
                                           contain1d (min1, max1, min2, max2) = min1 <= min2
                                                                                && max1 >= max2

instance LoozeBoundingVolume AABB where
  loozen _          InfiniteAABB = InfiniteAABB
  loozen loozeWidth (AABB mn mx) = AABB (map ((-) loozeWidth) mn) (map (+ loozeWidth) mx)

aabbFromBounds :: [ (Double, Double) ] -> AABB
aabbFromBounds bounds = aabbFromBoundsUnsafe $ map rearrange bounds
                        where
                        rearrange p@(a, b) = if b < a then (b, a) else p

aabbFromBoundsUnsafe :: [ (Double, Double) ] -> AABB
aabbFromBoundsUnsafe = uncurry AABB . unzip
