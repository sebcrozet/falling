module Physics.Falling.RigidBody.RigidBodySolver
(
solveConstraintsIsland
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Math.OrthonormalBasis
import Physics.Falling.Collision.Collision
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.Shape.TransformableShape
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.RigidBody.OrderedRigidBody
import qualified Physics.Falling.Constraint.Solver.AccumulatedImpulse as AI

solveConstraintsIsland :: (OrthonormalBasis lv n
                           , Ord idt
                           , TransformSystem t lv av
                           , VolumetricShape dvt i ii av t
                           , TransformableShape dvt t dvt'
                           , TransformableShape svt t svt'
                           , UnitVector lv n) =>
                          Double ->
                          [ (Int, OrderedRigidBody idt t lv av i ii dvt svt dvt' svt') ] ->
                          [ ContactManifold lv n ] ->
                          ([(Int, OrderedRigidBody idt t lv av i ii dvt svt dvt' svt')], [ ContactManifold lv n ])
solveConstraintsIsland dt bodies contacts =
                       (newOrderedBodies, newManifolds)
                       where
                       dynamicBodies  = map (\(i, rb) -> (i, let DynamicBody db = rigidBody rb in db)) bodies
                       (newDynamicsBodies, newManifolds) = AI.solveConstraintsIsland dt dynamicBodies contacts
                       newOrderedBodies = zipWith (\(_, rb) (i, odb) -> (i, mapOnBody (\_ -> DynamicBody rb) odb))
                                                  newDynamicsBodies
                                                  bodies
