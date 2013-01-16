module Physics.Falling.RigidBody.Filters
(
islandNodeFilter
, islandEdgeFilter
, activeBodyFilter
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.RigidBody

islandNodeFilter :: (a, Int) -> Bool
islandNodeFilter (_, uid) = uid >= 0 -- FIXME

islandEdgeFilter :: (a, Int) -> Bool
islandEdgeFilter _ = True           -- FIXME

activeBodyFilter :: (Ord idt , Transform t lv av , VolumetricShape dvt i ii av t) =>
                    OrderedRigidBody idt t lv av i ii dvt svt -> Bool
activeBodyFilter b = case rigidBody b of
                     DynamicBody _ -> True
                     _             -> False
