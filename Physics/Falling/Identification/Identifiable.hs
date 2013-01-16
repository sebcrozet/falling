{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Physics.Falling.Identification.Identifiable
(
Identifiable(..)
)
where

class Ord idt => Identifiable rb idt | rb -> idt where
  identifier  :: rb -> idt
