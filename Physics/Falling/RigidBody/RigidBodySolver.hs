module Physics.Falling.RigidBody.RigidBodySolver
(
solveConstraintsIsland
)
where

import Data.Vect.Double.Base
import Physics.Falling.Math.Transform
import Physics.Falling.Collision.Collision
import Physics.Falling.Shape.VolumetricShape
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.RigidBody.OrderedRigidBody
import qualified Physics.Falling.Constraint.Solver.AccumulatedImpulse as AI

solveConstraintsIsland :: (Ord idt , TransformSystem t lv av , VolumetricShape dvt i ii av t, UnitVector lv n) =>
                          Double ->
                          [ (Int, OrderedRigidBody idt t lv av i ii dvt svt) ] ->
                          [ ContactManifold lv n ] ->
                          ([(Int, OrderedRigidBody idt t lv av i ii dvt svt)], [ ContactManifold lv n ])
solveConstraintsIsland dt bodies contacts =
                       (newOrderedBodies, newManifolds)
                       where
                       dynamicBodies  = map (\(i, rb) -> (i, let DynamicBody db = rigidBody rb in db)) bodies
                       (newDynamicsBodies, newManifolds) = AI.solveConstraintsIsland dt dynamicBodies contacts
                       newOrderedBodies = zipWith (\(_, rb) (i, odb) -> (i, mapOnBody (\_ -> DynamicBody rb) odb))
                                                  newDynamicsBodies
                                                  bodies
