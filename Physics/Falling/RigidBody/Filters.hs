module Physics.Falling.RigidBody.Filters
(
islandNodeFilter
, activeBodyFilter
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Collision.Detection.NarrowPhase
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.RigidBody

islandNodeFilter :: NarrowPhase nf rb cm => (nf, Int) -> Bool
islandNodeFilter e@(_, uid) = islandNarrowPhaseFilter e && uid >= 0

activeBodyFilter :: (Ord idt , TransformSystem t lv av , VolumetricShape dvt i ii av t) =>
                    OrderedRigidBody idt t lv av i ii dvt svt -> Bool
activeBodyFilter b = case rigidBody b of
                     DynamicBody _ -> True
                     _             -> False
