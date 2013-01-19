module Physics.Falling.Constraint.Solver.AccumulatedImpulse
(
solveConstraintsIsland
)
where

import Control.Monad.ST
import Data.Vect.Double.Base hiding(translation)
import qualified Data.Vector as V
import Data.Vector.Mutable hiding(length) -- FIXME: use unboxed vectors instead (implement instances for Unbox VecN)
import Physics.Falling.Math.Transform
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
                       (solveSystem 8 dt bodies indexShift $
                        foldr (addEquations dt bodiesVect indexShift) emptySystem contacts,
                        contacts)
                       where
                       bodiesVect    = initBodiesVect bodies indexShift numBodyIndex
                       firstB:otherB = bodies
                       (m, mm)       = foldr (\(i, _) (m', mm') -> (min m' i, max mm' i))
                                             (fst firstB, fst firstB)
                                             otherB
                       numBodyIndex  = mm - m + 1
                       indexShift    = -m

addEquations :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                Double ->
                V.Vector rb ->
                Int ->
                ContactManifold lv n ->
                AccumulatedImpulseSystem lv av ->
                AccumulatedImpulseSystem lv av
addEquations dt bodies shift cm sys = foldr (addEquation dt bodies shift) sys cm

addEquation :: (Dynamic rb t lv av ii, UnitVector lv n) =>
               Double ->
               V.Vector rb ->
               Int ->
               Collision lv n ->
               AccumulatedImpulseSystem lv av ->
               AccumulatedImpulseSystem lv av
addEquation dt bodies shift (UnibodyCollision idx coll _) sys =
            addSingleBodyEquation conf sid 0.0 infinity (depth / dt) sys -- FIXME: uggly to set limits and depth here
            where
            sid   = idx + shift
            rb    = bodies V.! sid
            conf  = configureContact dt rb coll False
            depth = penetrationDepth coll
addEquation dt bodies shift (BibodyCollision  id1 id2 coll _) sys =
            addTwoBodiesEquation conf1 conf2 sid1 sid2 0.0 infinity (depth / dt) sys -- FIXME: uggly to set limits and depth here
            where
            sid1  = id1 + shift
            sid2  = id2 + shift
            rb1   = bodies V.! sid1
            rb2   = bodies V.! sid2
            conf1 = configureContact dt rb1 coll True
            conf2 = configureContact dt rb2 coll False
            depth = penetrationDepth coll

-- FIXME: also generate equations for friction
-- FIXME: handle restitution
-- FIXME: handle penetration correction
configureContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                    Double -> rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av
configureContact dt rb (CollisionDescr c n _) invN =
                 contactConfiguration (translation $ getLocalToWorld rb)
                                      (getLinearVelocity  rb &+ dt *& getExternalLinearForce rb)
                                      (getAngularVelocity rb &+ dt *& getExternalAngularForce rb)
                                      (getInverseMass rb)
                                      (getWorldSpaceInverseInertiaTensor rb)
                                      (if invN then neg (fromNormal n) else (fromNormal n))
                                      c
  
initBodiesVect :: [ (Int, rb) ] -> Int -> Int -> (V.Vector rb)
initBodiesVect bodies indexShift numBodyIndex = runST $ do
                        bodyVect <- new numBodyIndex
                        _ <- mapM_ (\(i, b) -> write bodyVect (i + indexShift) b) $ bodies
                        V.unsafeFreeze bodyVect

solveSystem :: (Dynamic rb t lv av ii) =>
               Int -> Double -> [ (Int, rb) ] -> Int -> AccumulatedImpulseSystem lv av -> [ (Int, rb) ]
solveSystem niter dt bodies shift sys = case solve niter sys of
                                        Nothing             -> bodies
                                        Just (lvect, avect) -> integrate dt bodies lvect avect shift

-- NOTE: this step will fail with an index out of range if a body without collision is in the
-- system
integrate :: (Dynamic rb t lv av ii) =>
             Double -> [ (Int, rb) ] -> V.Vector lv -> V.Vector av -> Int -> [ (Int, rb) ]
integrate dt bodies linImpVect angImpVect shift =
          map applyBodyImpulses bodies
          where
          applyBodyImpulses (i, rb) =
            let idx    = i + shift in
            let linImp = linImpVect V.! idx in
            let angImp = angImpVect V.! idx in
            let rb'    = if (isNaN $ len linImp) || (isNaN $ len angImp) then error "NaN integrate" else setVelocities
                         (getLinearVelocity  rb &+ getExternalLinearForce  rb &* dt,
                          getAngularVelocity rb &+ getExternalAngularForce rb &* dt)
                         rb
            in
            (i, applyImpulses linImp angImp rb')
