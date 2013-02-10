module Physics.Falling.Math.Error
(
infinity
, sqEpsRel
, epsTol
, margin
, gap
)
where

infinity :: Double
infinity = 1.0 / 0.0

sqEpsRel :: Double
sqEpsRel = sqrt epsTol

epsTol :: Double
epsTol = 100.0 * (2.0 ** (-53))

margin :: Double
margin = 0.04

gap :: Double
gap = 2.0 * margin
