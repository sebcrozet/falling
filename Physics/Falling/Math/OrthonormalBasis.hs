{-# LANGUAGE MultiParamTypeClasses #-}

module Physics.Falling.Math.OrthonormalBasis
(
OrthonormalBasis(..)
)
where

import Physics.Falling.Math.Transform

class (UnitVector v n) => OrthonormalBasis v n where
  canonicalBasis :: [ n ]
  completeBasis  :: n -> (n, [ n ])
