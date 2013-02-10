module Physics.Falling.Constraint.Solver.FirstOrderWorldAccumulatedImpulse
(
addFirstOrderEquations
, firstOrderIntegrate
)
where

import qualified Data.Vector as V
import Physics.Falling.Math.Transform
import Physics.Falling.Math.Error
import Physics.Falling.Collision.Collision
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.Constraint.Solver.ContactDynamicConfiguration hiding(linearVelocity, angularVelocity)
import Physics.Falling.Constraint.Solver.AccumulatedImpulseSystem

-- FIXME: instead of all that, define a type «FirstOrderWorld» and «SecondOrderWorld» and add
-- contact to them, and let them solve themselves


addFirstOrderEquations :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double ->
                          V.Vector rb ->
                          Int ->
                          ContactManifold lv n ->
                          AccumulatedImpulseSystem lv av ->
                          AccumulatedImpulseSystem lv av
addFirstOrderEquations dt bodies shift cm sys =
                       foldr (addFirstOrderEquation dt bodies shift) sys cm

addFirstOrderEquation :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                         Double ->
                         V.Vector rb ->
                         Int ->
                         Collision lv n ->
                         AccumulatedImpulseSystem lv av ->
                         AccumulatedImpulseSystem lv av
addFirstOrderEquation dt bodies shift (UnibodyCollision idx coll _) sys =
                      foldr (\conf s -> addSingleBodyEquation conf sid 0.0 infinity (err / dt) s) sys confs -- FIXME: uggly to set limits and depth here
                      where
                      sid   = idx + shift
                      rb    = bodies V.! sid
                      confs = firstOrderWorldContact dt rb coll False
                      depth = penetrationDepth coll
                      err   = firstOrderErrorEstimator depth
addFirstOrderEquation dt bodies shift (BibodyCollision  id1 id2 coll _) sys =
                      foldr (\(conf1, conf2) s -> addTwoBodiesEquation conf1 conf2 sid1 sid2 0.0 infinity (err / dt) s) sys $ zip confs1 confs2 -- FIXME: uggly to set limits and depth here
                      where
                      sid1   = id1 + shift
                      sid2   = id2 + shift
                      rb1    = bodies V.! sid1
                      rb2    = bodies V.! sid2
                      confs1 = firstOrderWorldContact dt rb1 coll True
                      confs2 = firstOrderWorldContact dt rb2 coll False
                      depth  = penetrationDepth         coll
                      err    = firstOrderErrorEstimator depth



firstOrderErrorEstimator :: Double -> Double
firstOrderErrorEstimator penDepth = max 0.0 $ penDepth - gap

firstOrderWorldContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double -> rb -> CollisionDescr lv n -> Bool -> [ ContactDynamicConfiguration lv av ]
firstOrderWorldContact _ rb (CollisionDescr c _ _ n _) invN =
                       [ contactConfiguration (translation $ localToWorld rb)
                                              zero
                                              zero
                                              (inverseMass rb)
                                              (worldSpaceInverseInertiaTensor rb)
                                              (if invN then neg (fromNormal n) else (fromNormal n))
                                              c ]

-- Note: this step will fail with an index out of range if a body without collision is in the
-- system
firstOrderIntegrate :: (Dynamic rb t lv av ii) =>
                       Double -> [ (Int, rb) ] -> V.Vector lv -> V.Vector av -> Int -> [ (Int, rb) ]
firstOrderIntegrate dt bodies linImpVect angImpVect shift =
          map applyBodyImpulses bodies
          where
          applyBodyImpulses (i, rb) =
            let idx    = i + shift in
            let linImp = linImpVect V.! idx in
            let angImp = angImpVect V.! idx in
            (i, applyPositionImpulses (dt *& linImp) (dt *& angImp) rb)
