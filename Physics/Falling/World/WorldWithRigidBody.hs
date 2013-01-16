{-# LANGUAGE FlexibleContexts #-}

module Physics.Falling.World.WorldWithRigidBody
(
World
, mkWorld
)
where

import Physics.Falling.Math.Transform
import qualified Physics.Falling.World.GenericWorld as GW
import Physics.Falling.Collision.Detection.BroadPhase
import Physics.Falling.Collision.Detection.NarrowPhase
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.Filters
import Physics.Falling.Identification.SignedIndexGenerator

type World transformType
           linearVelocityType
           angularVelocityType
           inertiaTensorType
           inverseInertiaTensorType
           dynamicCollisionVolumeType
           staticCollisionVolumeType
           broadPhase
           narrowPhase -- FIXME: remove that?
           contactManifoldType
           identifierType
           = GW.World (OrderedRigidBody identifierType
                                        transformType
                                        linearVelocityType
                                        angularVelocityType
                                        inertiaTensorType
                                        inverseInertiaTensorType
                                        dynamicCollisionVolumeType
                                        staticCollisionVolumeType)
                      broadPhase
                      narrowPhase
                      contactManifoldType
                      SignedIndexGenerator
                      identifierType

mkWorld :: (Ord idt
            , Transform t lv av
            , VolumetricShape ds i ii av t
            , NarrowPhase nf (OrderedRigidBody idt t lv av i ii ds ss) cm
            , BroadPhase  bf (OrderedRigidBody idt t lv av i ii ds ss)) =>
           bf ->
           (OrderedRigidBody idt t lv av i ii ds ss -> OrderedRigidBody idt t lv av i ii ds ss -> nf) ->
           (Double -> [ (Int, OrderedRigidBody idt t lv av i ii ds ss) ] -> [ cm ] ->
             ([ (Int, OrderedRigidBody idt t lv av i ii ds ss) ], [ cm ])) ->
           World t lv av i ii ds ss bf nf cm idt
mkWorld initBroadPhase initCollisionDetectionDispatcher initConstraintsIslandSolver
        = GW.mkWorld initBroadPhase
                     initCollisionDetectionDispatcher
                     initConstraintsIslandSolver
                     islandNodeFilter
                     islandEdgeFilter
                     activeBodyFilter
                     newGenerator
