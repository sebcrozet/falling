module Physics.Falling.Constraint.Solvable
(
)
where

class Solvable cm where
  init  :: cm -> c
  solve :: [  c ] -> [ c ]
