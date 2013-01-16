module Physics.Falling.Shape.AABB
(
AABB(..),
-- intersectsAABB
)
where

-- import Physics.Falling.Shape.ExtraTransform
-- import Physics.Falling.Shape.Shape

data AABB = AABB [ (Double, Double) ]
          | InfiniteAABB


-- intersectsAABB :: AABB -> AABB -> Bool
-- intersectsAABB InfiniteAABB _ = True
-- intersectsAABB _ InfiniteAABB = True
-- intersectsAABB (AABB bounds1) (AABB bounds2) = or $ zipWith rangesIntersect bounds1 bounds2
--                                                where
--                                                rangesIntersect (a, a') (b, b') = not ((b > a') || (b' < a))
-- 
-- aabbFromShape :: (PrincipalDirections v) => Shape v -> AABB
-- aabbFromShape (InfiniteShape _) = InfiniteAABB
-- aabbFromShape (FiniteShape g)   = InfiniteAABB
--                                         where
--                                         supportAxis :: [ v ]
--                                         supportAxis = principalDirections
