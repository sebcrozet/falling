module Physics.Falling.Shape.AABBFactory
(
aabbFromImplicitShape
, planeAABB
, ballAABB
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Math.OrthonormalBasis
import Physics.Falling.Shape.AABB
import Physics.Falling.Shape.ImplicitShape
import Physics.Falling.Shape.Plane
import Physics.Falling.Shape.Ball

aabbFromImplicitShape :: (ImplicitShape g v, OrthonormalBasis v n, Transform m v) =>
                         g -> m -> AABB
aabbFromImplicitShape shape t = aabbFromBoundsUnsafe $ map directionalBounds canonicalBasis
                                where
                                directionalBounds dir = let dirv = fromNormal dir in
                                                        (dirv &.supportPointWithTransform shape (neg dirv) t
                                                        , dirv &. supportPointWithTransform shape dirv t)

planeAABB :: (Vector v, UnitVector v n) => Plane v n -> AABB
planeAABB _ = InfiniteAABB

ballAABB :: (OrthonormalBasis v n) => Ball v -> AABB
ballAABB (Ball center radius) = aabbFromBoundsUnsafe $ map directionalBounds canonicalBasis
                                where
                                directionalBounds dir = let dirv    = fromNormal dir in
                                                        let centerv = center &. dirv in
                                                        (centerv - radius, centerv + radius)
