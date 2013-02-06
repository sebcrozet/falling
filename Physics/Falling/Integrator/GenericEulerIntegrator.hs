module Physics.Falling.Integrator.GenericEulerIntegrator
(
integrateBodyPosition
, integrateBodyVelocity
, displacement
, integrateVelocity
, integrate
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.RigidBody.Positionable hiding(translate, rotate)
import Physics.Falling.RigidBody.Dynamic

dampVector :: (Vector v) => v -> v
dampVector =  (*&) 0.9998

integrateBodyPosition :: (Dynamic b t lv av ii) =>
                         Double -> b -> b
integrateBodyPosition dt b = appendTransform displacementMatrix b
                             where
                             l2w                = localToWorld b
                             displacementMatrix = displacement dt
                                                               (linearVelocity  b)
                                                               (angularVelocity b)
                                                               l2w


integrateBodyVelocity :: (Dynamic b t lv av ii) =>
                         Double -> b -> b
integrateBodyVelocity dt b = setVelocities newVels b
                             where
                             oldLinVel = linearVelocity b
                             oldAngVel = angularVelocity b
                             fextLin   = externalLinearForce b
                             fextAng   = externalAngularForce b
                             newVels   = integrateVelocity dt fextLin fextAng oldLinVel oldAngVel

displacement :: (Vector lv, Vector av, TransformSystem m lv av) =>
                 Double -> lv -> av -> m -> m
displacement dt linVel angVel originalMatrix = translate (dt *& linVel)
                                               $ rotateWrtPoint (dt *& angVel) originalTranslation
                                               $ idmtx
                                               where
                                               originalTranslation = translation originalMatrix
                            

integrateVelocity :: (Vector v, Vector av) => Double -> v -> av -> v -> av -> (v, av)
integrateVelocity dt fextLin fextAng linVel angVel =
                  ( dampVector newLinearVelocity, dampVector newAngularVelocity )
                  where
                  newLinearVelocity  = integrate dt linVel fextLin
                  newAngularVelocity = integrate dt angVel fextAng

integrate :: (Vector v) => Double -> v -> v -> v
integrate dt v f = v &+ dt *& f
