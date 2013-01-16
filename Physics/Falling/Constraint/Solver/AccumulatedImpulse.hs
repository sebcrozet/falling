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
                        foldr (addEquations bodiesVect indexShift) emptySystem contacts,
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
                V.Vector rb ->
                Int ->
                ContactManifold lv n ->
                AccumulatedImpulseSystem lv av ->
                AccumulatedImpulseSystem lv av
addEquations bodies shift cm sys = foldr (addEquation bodies shift) sys cm

addEquation :: (Dynamic rb t lv av ii, UnitVector lv n) =>
               V.Vector rb ->
               Int ->
               Collision lv n ->
               AccumulatedImpulseSystem lv av ->
               AccumulatedImpulseSystem lv av
addEquation bodies shift (UnibodyCollision idx coll _) sys =
            addSingleBodyEquation conf sid 0.0 infinity sys -- FIXME: uggly to set limits here
            where
            sid  = idx + shift
            rb   = bodies V.! sid
            conf = configureContact rb coll False
addEquation bodies shift (BibodyCollision  id1 id2 coll _) sys =
            addTwoBodiesEquation conf1 conf2 sid1 sid2 0.0 infinity sys -- FIXME: uggly to set limits here
            where
            sid1  = id1 + shift
            sid2  = id2 + shift
            rb1   = bodies V.! sid1
            rb2   = bodies V.! sid2
            conf1 = configureContact rb1 coll False
            conf2 = configureContact rb2 coll True

-- FIXME: also generate equations for friction
-- FIXME: handle restitution
-- FIXME: handle penetration correction
configureContact :: (Dynamic rb t lv av ii, UnitVector lv n) =>
                    rb -> CollisionDescr lv n -> Bool -> ContactDynamicConfiguration lv av
configureContact rb (CollisionDescr c n _) invN =
                 contactConfiguration (translation $ getLocalToWorld rb)
                                      (getLinearVelocity rb) -- FIXME: add external forces?
                                      (getAngularVelocity rb) -- FIXME: add external forces?
                                      (getInverseMass rb)
                                      (getWorldSpaceInverseInertiaTensor rb)
                                      (if invN then neg (fromNormal n) else (fromNormal n))
                                      c
  
initBodiesVect :: [ (Int, rb) ] -> Int -> Int -> (V.Vector rb)
initBodiesVect bodies indexShift numBodyIndex = runST $ do
                        bodyVect <- new numBodyIndex
                        _ <- mapM_ (\(i, b) -> unsafeWrite bodyVect (i + indexShift) b) $ bodies
                        V.unsafeFreeze bodyVect

solveSystem :: (Dynamic rb t lv av ii) =>
               Int -> Double -> [ (Int, rb) ] -> Int -> AccumulatedImpulseSystem lv av -> [ (Int, rb) ]
solveSystem niter dt bodies shift sys = let (lvect, avect) = solve niter sys in
                                        integrate dt bodies lvect avect shift

integrate :: (Dynamic rb t lv av ii) =>
             Double -> [ (Int, rb) ] -> V.Vector lv -> V.Vector av -> Int -> [ (Int, rb) ]
integrate dt bodies linImpVect angImpVect shift =
          map applyBodyImpulses bodies
          where
          applyBodyImpulses (i, rb) =
            let idx    = i + shift                    in
            let linImp = linImpVect V.! idx in
            let angImp = angImpVect V.! idx in
            let rb'    = setVelocities
                         (getLinearVelocity  rb &+ getExternalLinearForce  rb &* dt,
                          getAngularVelocity rb &+ getExternalAngularForce rb &* dt)
                         rb
            in
            (i, applyImpulses linImp angImp rb')
