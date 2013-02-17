module Physics.Falling.Constraint.Solver.SecondOrderWorldAccumulatedImpulse
(
addSecondOrderEquations
, secondOrderIntegrate
)
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Physics.Falling.Math.Transform
import Physics.Falling.Math.Error
import Physics.Falling.Math.OrthonormalBasis
import Physics.Falling.Collision.Collision
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.Constraint.Solver.ContactDynamicConfiguration hiding(linearVelocity, angularVelocity)
import Physics.Falling.Constraint.Solver.AccumulatedImpulseSystem


secondOrderErrorEstimator :: Double -> Double
secondOrderErrorEstimator penDepth = max 0.0 $ (min gap penDepth) - 0.01

addSecondOrderEquations :: (OrthonormalBasis lv n, Dynamic rb t lv av ii, UnitVector lv n) =>
                           Double ->
                           V.Vector rb ->
                           Int ->
                           ContactManifold lv n ->
                           AccumulatedImpulseSystem lv av ->
                           AccumulatedImpulseSystem lv av
addSecondOrderEquations dt bodies shift cm sys =
                        foldr (addSecondOrderEquation dt bodies shift ) sys cm

addSecondOrderEquation :: (OrthonormalBasis lv n, Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double ->
                          V.Vector rb ->
                          Int ->
                          Collision lv n ->
                          AccumulatedImpulseSystem lv av ->
                          AccumulatedImpulseSystem lv av
addSecondOrderEquation dt bodies shift (UnibodyCollision idx coll _) sys =
                       foldr (\(ll, ul, e, conf) s -> addSingleBodyEquation conf sid ll ul (e / dt) s) sys confs -- FIXME: uggly to set limits and depth here
                       where
                       sid    = idx + shift
                       rb     = bodies V.! sid
                       confs  = (0.0, infinity, err, secondOrderWorldContact dt rb center normal False)
                                : map (\a -> (-0.7 * 9.81, 0.7 * 9.81, 0.0, a))
                                      (map (\t -> secondOrderWorldContact dt rb center t False) tangeants)
                       depth  = penetrationDepth coll
                       err    = secondOrderErrorEstimator depth
                       normal = contactNormal coll
                       center = contactCenter coll
                       tangeants = snd $ completeBasis normal
addSecondOrderEquation dt bodies shift (BibodyCollision  id1 id2 coll _) sys =
                       foldr (\(ll, ul, e, conf1, conf2) s -> addTwoBodiesEquation conf1 conf2 sid1 sid2 ll ul (e / dt) s) sys confs -- FIXME: uggly to set limits and depth here
                       where
                       sid1      = id1 + shift
                       sid2      = id2 + shift
                       rb1       = bodies V.! sid1
                       rb2       = bodies V.! sid2
                       confs     = (0.0, infinity, err, secondOrderWorldContact dt rb1 center normal True, secondOrderWorldContact dt rb2 center normal False)
                                   : map (\(a, b) -> (-0.7 * 9.81, 0.7 * 9.81, 0.0, a, b))
                                         (zip (map (\t -> secondOrderWorldContact dt rb1 center t True) tangeants)
                                              (map (\t -> secondOrderWorldContact dt rb2 center t False) tangeants))
                       depth     = penetrationDepth coll
                       err       = secondOrderErrorEstimator depth
                       normal    = contactNormal coll
                       center    = contactCenter coll
                       tangeants = snd $ completeBasis normal

secondOrderWorldContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                           Double -> rb -> lv -> n -> Bool -> ContactDynamicConfiguration lv av
secondOrderWorldContact dt rb c n invN =
                        contactConfiguration (translation $ localToWorld rb)
                                             (linearVelocity  rb &+ dt *& externalLinearForce rb)
                                             (angularVelocity rb &+ dt *& externalAngularForce rb)
                                             (inverseMass rb)
                                             (worldSpaceInverseInertiaTensor rb)
                                             (if invN then neg (fromNormal n) else (fromNormal n))
                                             c

secondOrderIntegrate :: (Dynamic rb t lv av ii, UV.Unbox lv, UV.Unbox av) =>
                        Double -> [ (Int, rb) ] -> UV.Vector lv -> UV.Vector av -> Int -> [ (Int, rb) ]
secondOrderIntegrate dt bodies linImpVect angImpVect shift =
          map applyBodyImpulses bodies
          where
          applyBodyImpulses (i, rb) =
            let idx    = i + shift in
            let linImp = linImpVect UV.! idx in
            let angImp = angImpVect UV.! idx in
            let rb'    = setVelocities (linearVelocity  rb &+ externalLinearForce  rb &* dt,
                                        angularVelocity rb &+ externalAngularForce rb &* dt)
                                       rb
            in
            (i, applyImpulses linImp angImp rb')
