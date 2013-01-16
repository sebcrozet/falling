module Physics.Falling.Constraint.Solver.ContactDynamicConfiguration
(
ContactDynamicConfiguration(..)
, contactConfiguration
)
where

import Data.Vect.Double.Base
import Physics.Falling.Dynamics.InertiaTensor
import Physics.Falling.Math.Transform

data ContactDynamicConfiguration lv av = ContactDynamicConfiguration {
                                           linearDirection          :: lv
                                           , angularDirection       :: av
                                           , inverseLinearInertia   :: Double
                                           , inverseInertiaMomentum :: Double
                                           , linearVelocity         :: Double
                                           , angularVelocity        :: Double
                                         }

contactConfiguration :: (DotProd lv, DotProd av, PerpProd lv av, InverseInertiaTensor ii av t) =>
                        lv -> lv -> av -> Double -> ii -> lv -> lv -> ContactDynamicConfiguration lv av
contactConfiguration rbCenter
                     rbLinVelocity
                     rbAngVelocity
                     rbInvMass
                     rbWorldInvInertia
                     contactNormal
                     contactCenter =
                     ContactDynamicConfiguration {
                       linearDirection          = contactNormal
                       , angularDirection       = rotationAxis
                       , inverseLinearInertia   = if lensqr contactNormal /= 0.0 then rbInvMass else 0.0
                       , inverseInertiaMomentum = len (rbWorldInvInertia `applyToVector` rotationAxis) /
                                                  len rotationAxis
                       , linearVelocity         = rbLinVelocity &. contactNormal 
                       , angularVelocity        = rbAngVelocity &. rotationAxis
                     }
                     where
                     rotationAxis = (contactCenter &- rbCenter) `perp` contactNormal
