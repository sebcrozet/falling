{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Identification.IndexGenerator
(
IndexGenerator(..)
)
where

class IndexGenerator ig rb where
  generate     :: rb -> ig -> (ig, Int)
  recycle      :: rb -> Int -> ig -> ig
