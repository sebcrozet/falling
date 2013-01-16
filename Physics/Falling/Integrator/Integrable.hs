module Physics.Falling.Integrator.Integrable
(
Integrable(..)
)
where

class Integrable rb where
  integratePosition ::  Double -> rb -> rb
  integrateVelocity ::  Double -> rb -> rb
