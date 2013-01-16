module Physics.Falling.Constraint.Solver.AccumulatedImpulseSystem
(
AccumulatedImpulseSystem
, emptySystem
, addSingleBodyEquation
, addTwoBodiesEquation
, solve
)
where

import Control.Monad.ST
import qualified Control.Monad as CM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.STRef
import Data.Vect.Double.Base
import Physics.Falling.Constraint.Solver.ContactDynamicConfiguration

data Equation lv av = UnibodyEquation {
                        index                :: Int
                        , configuration      :: ContactDynamicConfiguration lv av
                        , velocityCorrection :: Double
                        , invTotalInvInertia :: Double
                        , lowLimit           :: Double
                        , higLimit           :: Double
                      }
                      | BibodiesEquation {
                        index1               :: Int
                        , index2             :: Int
                        , configuration1     :: ContactDynamicConfiguration lv av
                        , configuration2     :: ContactDynamicConfiguration lv av
                        , velocityCorrection :: Double
                        , invTotalInvInertia :: Double
                        , lowLimit           :: Double
                        , higLimit           :: Double
                      }

data AccumulatedImpulseSystem lv av = AccumulatedImpulseSystem Int [ Equation lv av ]

emptySystem :: AccumulatedImpulseSystem lv av
emptySystem = AccumulatedImpulseSystem 0 []

addSingleBodyEquation :: ContactDynamicConfiguration lv av ->
                         Int ->
                         Double ->
                         Double ->
                         AccumulatedImpulseSystem lv av ->
                         AccumulatedImpulseSystem lv av
addSingleBodyEquation conf idx lLimit hLimit (AccumulatedImpulseSystem mid impulseSystem) =
                      AccumulatedImpulseSystem (max mid idx) (newEquation:impulseSystem)
                      where
                      newEquation = UnibodyEquation {
                                      index                = idx
                                      , configuration      = conf
                                      , velocityCorrection = linearVelocity conf +
                                                             angularVelocity conf
                                      , invTotalInvInertia = 1.0 / (inverseLinearInertia conf +
                                                                    inverseInertiaMomentum conf)
                                      , lowLimit           = lLimit
                                      , higLimit           = hLimit
                                    }

addTwoBodiesEquation :: ContactDynamicConfiguration lv av ->
                        ContactDynamicConfiguration lv av ->
                        Int ->
                        Int ->
                        Double ->
                        Double ->
                        AccumulatedImpulseSystem lv av ->
                        AccumulatedImpulseSystem lv av
addTwoBodiesEquation conf1 conf2 id1 id2 lLimit hLimit (AccumulatedImpulseSystem mid impulseSystem) =
                     AccumulatedImpulseSystem (max mid $ max id1 id2) (newEquation:impulseSystem)
                     where
                     newEquation = BibodiesEquation {
                                     index1               = id1
                                     , index2             = id2
                                     , configuration1     = conf1
                                     , configuration2     = conf2
                                     , velocityCorrection = linearVelocity conf1  +
                                                            angularVelocity conf1 +
                                                            linearVelocity conf2  +
                                                            angularVelocity conf2
                                     , invTotalInvInertia = 1.0 / (inverseLinearInertia conf1   +
                                                                   inverseInertiaMomentum conf1 +
                                                                   inverseLinearInertia conf2   +
                                                                   inverseInertiaMomentum conf2)
                                     , lowLimit           = lLimit
                                     , higLimit           = hLimit
                                   }

solve :: (Vector lv, Vector av, DotProd lv, DotProd av) =>
         Int -> AccumulatedImpulseSystem lv av -> (V.Vector lv, V.Vector av)
solve niter system = runST $ solveST niter system

solveST :: (Vector lv, Vector av, DotProd lv, DotProd av) =>
           Int -> AccumulatedImpulseSystem lv av -> ST s (V.Vector lv, V.Vector av)
solveST niter (AccumulatedImpulseSystem mid impulseSystem) =
        do
        lVect   <- M.replicate (mid + 1) zero -- FIXME: can have a huge memory cost
        aVect   <- M.replicate (mid + 1) zero -- FIXME: can have a huge memory cost
        impVect <- M.replicate (length impulseSystem) 0.0
        currId  <- newSTRef 0 
        CM.forM_ [0 .. niter] $ (\_ -> do
          _   <- writeSTRef currId 0
          CM.forM_ impulseSystem $ (\equation -> do
            i   <- readSTRef currId
            _   <- modifySTRef currId (+1)
            acc <- M.read impVect i
            case equation of
              BibodiesEquation id1 id2 conf1 conf2 vc iti lb ub -> do
                lv1 <- M.read lVect id1
                av1 <- M.read aVect id1
                lv2 <- M.read lVect id2
                av2 <- M.read aVect id2
                let (lv1', av1', lv2', av2', acc') = solveBibodyConstraint lv1 av1 ld1 ad1 il1 ia1
                                                                           lv2 av2 ld2 ad2 il2 ia2
                                                                           iti vc  lb  ub  acc
                _ <- M.write impVect i acc'
                _ <- M.write lVect id1 lv1'
                _ <- M.write aVect id1 av1'
                _ <- M.write lVect id2 lv2'
                M.write aVect id2 av2'
                where
                ld1   = linearDirection conf1
                ad1   = angularDirection conf1
                il1   = inverseLinearInertia conf1
                ia1   = inverseInertiaMomentum conf1
                ld2   = linearDirection conf2
                ad2   = angularDirection conf2
                il2   = inverseLinearInertia conf2
                ia2   = inverseInertiaMomentum conf2
              UnibodyEquation  ie conf vc iti lb ub -> do
                lv <- M.read lVect ie
                av <- M.read aVect ie
                let (lv', av', acc') = solveUnibodyConstraint lv  av ld ad il  ia
                                                              iti vc lb ub acc
                _ <- M.write impVect i  acc'
                _ <- M.write lVect   ie lv'
                M.write aVect   ie av'
                where
                ld   = linearDirection conf
                ad   = angularDirection conf
                il   = inverseLinearInertia conf
                ia   = inverseInertiaMomentum conf))
        reslVect <- V.unsafeFreeze lVect
        resaVect <- V.unsafeFreeze aVect

        return (reslVect, resaVect)

solveBibodyConstraint :: (Vector lv, Vector av, DotProd lv, DotProd av) =>
                         lv -> av ->
                         lv -> av ->
                         Double -> Double ->
                         lv -> av ->
                         lv -> av ->
                         Double -> Double ->
                         Double ->
                         Double ->
                         Double -> Double ->
                         Double ->
                         (lv, av, lv, av, Double)
solveBibodyConstraint linearVelocity1   angularVelocity1 
                      linearDirection1  angularDirection1 
                      invLinearInertia1 invAngularInertia1
                      linearVelocity2   angularVelocity2 
                      linearDirection2  angularDirection2 
                      invLinearInertia2 invAngularInertia2
                      invTotalInertia
                      totalVelocityCorrection
                      impulseLowerBound impulseUpperBound
                      accumulatedImpulse =
                      (
                        linearVelocity1  &+ linearDirection1  &* correctiveImpulse,
                        angularVelocity1 &+ angularDirection1 &* correctiveImpulse,
                        linearVelocity2  &+ linearDirection2  &* correctiveImpulse,
                        angularVelocity2 &+ angularDirection2 &* correctiveImpulse,
                        newAccumulatedImpulse
                      )
                      where
                      deltaVelocity = invLinearInertia1  * (linearDirection1  &. linearVelocity1) +
                                      invAngularInertia1 * (angularDirection1 &. angularVelocity1) +
                                      invLinearInertia2  * (linearDirection2  &. linearVelocity2) +
                                      invAngularInertia2 * (angularDirection2 &. angularDirection2);
                      (newAccumulatedImpulse, correctiveImpulse) = solveClamp deltaVelocity
                                                                              totalVelocityCorrection
                                                                              invTotalInertia
                                                                              accumulatedImpulse
                                                                              impulseLowerBound
                                                                              impulseUpperBound

solveUnibodyConstraint :: (Vector lv, Vector av, DotProd lv, DotProd av) =>
                          lv -> av ->
                          lv -> av ->
                          Double -> Double ->
                          Double ->
                          Double ->
                          Double -> Double ->
                          Double ->
                          (lv, av, Double)
solveUnibodyConstraint linVel           angVel
                       linDir           angDir
                       invLinearInertia invAngularInertia
                       invTotalInertia
                       totalVelocityCorrection
                       impulseLowerBound impulseUpperBound
                       accumulatedImpulse =
                       (
                         linVel &+ linDir &* correctiveImpulse,
                         angVel &+ angDir &* correctiveImpulse,
                         newAccumulatedImpulse
                       )
                       where
                       deltaVelocity = invLinearInertia  * (linDir &. linVel)  +
                                       invAngularInertia * (angDir &. angVel)
                       (newAccumulatedImpulse, correctiveImpulse) = solveClamp deltaVelocity
                                                                               totalVelocityCorrection
                                                                               invTotalInertia
                                                                               accumulatedImpulse
                                                                               impulseLowerBound
                                                                               impulseUpperBound

solveClamp :: Double -> Double -> Double -> Double -> Double -> Double -> (Double, Double)
solveClamp value objective factor solution lowerBound upperBound =
           (clampedSolution, clampedCorrection)
           where
           remainingError = (objective - value) * factor
           candidateSolution = solution + remainingError
           clampedSolution = max lowerBound $
                             min upperBound candidateSolution
           clampedCorrection = clampedSolution - candidateSolution