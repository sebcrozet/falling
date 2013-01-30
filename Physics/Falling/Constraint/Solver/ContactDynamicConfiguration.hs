module Physics.Falling.Constraint.Solver.ContactDynamicConfiguration
(
ContactDynamicConfiguration(..)
, contactConfiguration
)
where

import Physics.Falling.Dynamics.InertiaTensor
import Physics.Falling.Math.Transform

data ContactDynamicConfiguration lv av = ContactDynamicConfiguration {
                                           linearDirection            :: lv
                                           , weightedLinearDirection  :: lv
                                           , angularDirection         :: av
                                           , weightedAngularDirection :: av
                                           , inverseLinearInertia     :: Double
                                           , inverseInertiaMomentum   :: Double
                                           , linearVelocity           :: Double
                                           , angularVelocity          :: Double
                                         } deriving(Show)

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
                       linearDirection            = contactNormal
                       , weightedLinearDirection  = invLinInertia *& contactNormal
                       , angularDirection         = rotationAxis
                       , weightedAngularDirection = iangDir
                       , inverseLinearInertia     = invLinInertia
                       , inverseInertiaMomentum   = if lensqr rotationAxis /= 0.0 then
                                                       iangDir &. rotationAxis
                                                    else
                                                       0.0
                       , linearVelocity           = rbLinVelocity &. contactNormal 
                       , angularVelocity          = rbAngVelocity &. rotationAxis
                     }
                     where
                     rotationAxis     = (contactCenter &- rbCenter) `perp` contactNormal
                     invLinInertia    = if lensqr contactNormal /= 0.0 then rbInvMass else 0.0
                     iangDir          = rbWorldInvInertia `applyToVector` rotationAxis
