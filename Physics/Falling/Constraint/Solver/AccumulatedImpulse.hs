module Physics.Falling.Constraint.Solver.AccumulatedImpulse
(
solveConstraintsIsland
)
where

import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Mutable hiding(length) -- FIXME: use unboxed vectors instead (implement instances for Unbox VecN)
import Physics.Falling.Math.Transform
import Physics.Falling.Math.OrthonormalBasis
import Physics.Falling.Collision.Collision
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.Constraint.Solver.AccumulatedImpulseSystem
import Physics.Falling.Constraint.Solver.FirstOrderWorldAccumulatedImpulse
import Physics.Falling.Constraint.Solver.SecondOrderWorldAccumulatedImpulse

solveConstraintsIsland :: (OrthonormalBasis lv n, Dynamic rb t lv av ii, UnitVector lv n) =>
                          Double ->
                          [ (Int, rb) ] ->
                          [ ContactManifold lv n ] ->
                          ([ (Int, rb) ], [ ContactManifold lv n ])
solveConstraintsIsland dt bodies contacts =
  (solveSystem 8 dt projectedBodies indexShift firstOrderIntegrate $
   foldr (addFirstOrderEquations dt projectedBodiesVect indexShift)
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
                           foldr (addSecondOrderEquations dt bodiesVect indexShift) 
                                 emptySystem
                                 contacts
                           , contacts)
  projectedBodiesVect     = initBodiesVect projectedBodies indexShift numBodyIndex


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
