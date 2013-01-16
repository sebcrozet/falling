{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Physics.Falling.Collision.Detection.BruteForceBroadPhase
(
BruteForceBroadPhase
, mkBroadPhase
)
where

import Data.List
import Physics.Falling.Collision.Detection.BroadPhase

data Eq rb => BruteForceBroadPhase rb = BruteForceBroadPhase ([rb], [(rb, rb)])

instance Eq rb => BroadPhase (BruteForceBroadPhase rb) rb where
  addBody    b (BruteForceBroadPhase (s, i)) = BruteForceBroadPhase (b:s, [ (b', b) | b' <- s ] ++ i)
  removeBody b (BruteForceBroadPhase (s, i)) = BruteForceBroadPhase (delete b s,
                                                                     [ e | e@(c, d) <- i, c /= b && d /= b ])
  update     _ (BruteForceBroadPhase (s, i)) = (i, [], BruteForceBroadPhase (s, []))

mkBroadPhase :: Eq rb => BruteForceBroadPhase rb
mkBroadPhase = BruteForceBroadPhase ([], [])
