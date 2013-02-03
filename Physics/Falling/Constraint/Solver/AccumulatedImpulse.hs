module Physics.Falling.Constraint.Solver.AccumulatedImpulse
(
solveConstraintsIsland
)
where

import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Mutable hiding(length) -- FIXME: use unboxed vectors instead (implement instances for Unbox VecN)
import Physics.Falling.Math.Transform
import Physics.Falling.Math.Error
import Physics.Falling.Collision.Collision
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.Constraint.Solver.AccumulatedImpulseSystem
import Physics.Falling.Constraint.Solver.ContactDynamicConfiguration

infinity :: Double
infinity = 1.0 / 0.0

solveConstraintsIsland :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double ->
                          [ (Int, rb) ] ->
                          [ ContactManifold lv n ] ->
                          ([ (Int, rb) ], [ ContactManifold lv n ])
solveConstraintsIsland dt bodies contacts =
  (solveSystem 8 dt projectedBodies indexShift firstOrderIntegrate $
   foldr (addEquations dt projectedBodiesVect indexShift firstOrderWorldContact firstOrderErrorEstimator)
         emptySystem
         contacts
   , contacts)
  where
  bodiesVect             = initBodiesVect bodies indexShift numBodyIndex
  firstB:otherB          = bodies
  (m, mm)                = foldr (\(i, _) (m', mm') -> (min m' i, max mm' i))
                                 (fst firstB, fst firstB)
                                 otherB
  numBodyIndex           = mm - m + 1
  indexShift             = -m
  (projectedBodies, _)   = (solveSystem 8 dt bodies indexShift secondOrderIntegrate $
                           foldr (addEquations dt bodiesVect indexShift
                                  secondOrderWorldContact secondOrderErrorEstimator) 
                                 emptySystem
                                 contacts
                           , contacts)
  projectedBodiesVect     = initBodiesVect projectedBodies indexShift numBodyIndex

addEquations :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                Double ->
                V.Vector rb ->
                Int ->
                (Double -> rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av) ->
                (Double -> Double) ->
                ContactManifold lv n ->
                AccumulatedImpulseSystem lv av ->
                AccumulatedImpulseSystem lv av
addEquations dt bodies shift configurator errorEstimator cm sys =
             foldr (addEquation dt bodies shift configurator errorEstimator) sys cm

addEquation :: (Dynamic rb t lv av ii, UnitVector lv n) =>
               Double ->
               V.Vector rb ->
               Int ->
               (Double -> rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av) ->
               (Double -> Double) ->
               Collision lv n ->
               AccumulatedImpulseSystem lv av ->
               AccumulatedImpulseSystem lv av
addEquation dt bodies shift configurator errorEstimator (UnibodyCollision idx coll _) sys =
            addSingleBodyEquation conf sid 0.0 infinity (err / dt) sys -- FIXME: uggly to set limits and depth here
            where
            sid   = idx + shift
            rb    = bodies V.! sid
            conf  = configurator dt rb coll False
            depth = penetrationDepth coll
            err   = errorEstimator   depth
addEquation dt bodies shift configurator errorEstimator (BibodyCollision  id1 id2 coll _) sys =
            addTwoBodiesEquation conf1 conf2 sid1 sid2 0.0 infinity (err / dt) sys -- FIXME: uggly to set limits and depth here
            where
            sid1  = id1 + shift
            sid2  = id2 + shift
            rb1   = bodies V.! sid1
            rb2   = bodies V.! sid2
            conf1 = configurator dt rb1 coll True
            conf2 = configurator dt rb2 coll False
            depth = penetrationDepth coll
            err   = errorEstimator   depth

  
initBodiesVect :: [ (Int, rb) ] -> Int -> Int -> (V.Vector rb)
initBodiesVect bodies indexShift numBodyIndex = runST $ do
                        bodyVect <- new numBodyIndex
                        _ <- mapM_ (\(i, b) -> write bodyVect (i + indexShift) b) $ bodies
                        V.unsafeFreeze bodyVect

solveSystem :: (Dynamic rb t lv av ii) =>
               Int                                                                             ->
               Double                                                                          ->
               [ (Int, rb) ]                                                                   ->
               Int                                                                             ->
               (Double -> [ (Int, rb) ] -> V.Vector lv -> V.Vector av -> Int -> [ (Int, rb) ]) ->
               AccumulatedImpulseSystem lv av ->
               [ (Int, rb) ]
solveSystem niter dt bodies shift integrator sys = case solve niter sys of
                                                   Nothing             -> bodies
                                                   Just (lvect, avect) -> integrator dt bodies lvect avect shift

-- FIXME: also generate equations for friction
-- FIXME: handle restitution

-- FIXME: instead of all that, define a type «FirstOrderWorld» and «SecondOrderWorld» and add
-- contact to them, and let them solve themselves
firstOrderErrorEstimator :: Double -> Double
firstOrderErrorEstimator penDepth = max 0.0 $ penDepth - margin

secondOrderErrorEstimator :: Double -> Double
secondOrderErrorEstimator penDepth = max 0.0 $ (min margin penDepth) - 0.01

secondOrderWorldContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                           Double -> rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av
secondOrderWorldContact dt rb (CollisionDescr c n _) invN =
                        contactConfiguration (translation $ getLocalToWorld rb)
                                             (getLinearVelocity  rb &+ dt *& getExternalLinearForce rb)
                                             (getAngularVelocity rb &+ dt *& getExternalAngularForce rb)
                                             (getInverseMass rb)
                                             (getWorldSpaceInverseInertiaTensor rb)
                                             (if invN then neg (fromNormal n) else (fromNormal n))
                                             c

firstOrderWorldContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double -> rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av
firstOrderWorldContact _ rb (CollisionDescr c n _) invN =
                       contactConfiguration (translation $ getLocalToWorld rb)
                                            zero
                                            zero
                                            (getInverseMass rb)
                                            (getWorldSpaceInverseInertiaTensor rb)
                                            (if invN then neg (fromNormal n) else (fromNormal n))
                                            c

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

secondOrderIntegrate :: (Dynamic rb t lv av ii) =>
                        Double -> [ (Int, rb) ] -> V.Vector lv -> V.Vector av -> Int -> [ (Int, rb) ]
secondOrderIntegrate dt bodies linImpVect angImpVect shift =
          map applyBodyImpulses bodies
          where
          applyBodyImpulses (i, rb) =
            let idx    = i + shift in
            let linImp = linImpVect V.! idx in
            let angImp = angImpVect V.! idx in
            let rb'    = setVelocities (getLinearVelocity  rb &+ getExternalLinearForce  rb &* dt,
                                        getAngularVelocity rb &+ getExternalAngularForce rb &* dt)
                                       rb
            in
            (i, applyImpulses linImp angImp rb')
