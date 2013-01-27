module Physics.Falling.Math.Error
(
sqEpsRel
, epsTol
, margin
)
where


sqEpsRel :: Double
sqEpsRel = sqrt epsTol

epsTol :: Double
epsTol = 100.0 * (2.0 ** (-53))

margin :: Double
margin = 0.04
