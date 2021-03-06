{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |An efficient implementation of 'Data.Graph.Inductive.Graph.Graph'
-- using big-endian patricia tree (i.e. "Data.IntMap").
--
-- This module provides the following specialised functions to gain
-- more performance, using GHC's RULES pragma:
--
-- * 'Physics.Falling.Collision.Graph.CollisionGraphTree.insNode'
--
-- * 'Physics.Falling.Collision.Graph.CollisionGraphTree.insEdge'
--
-- * 'Physics.Falling.Collision.Graph.CollisionGraphTree.gmap'
--
-- * 'Physics.Falling.Collision.Graph.CollisionGraphTree.nmap'
--
-- * 'Physics.Falling.Collision.Graph.CollisionGraphTree.emap'

module Physics.Falling.Collision.Graph.CollisionGraphTree
    ( Gr
    , UGr
    , fastInsNode
    , fastInsEdge
    , insertRemoveToMatch
    )
    where

import           Data.Graph.Inductive.Graph
import           Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Maybe

newtype Gr a b = Gr (GraphRep a b)

type GraphRep a b = IntMap (Context' a b)
type Context' a b = (IntMap b, a, IntMap b)

type UGr = Gr () ()

instance Graph Gr where
    -- required members
    empty           = Gr IM.empty
    isEmpty (Gr g)  = IM.null g
    match           = matchGr
    mkGraph vs es   = (insEdges' . insNodes vs) empty
        where
          insEdges' g = foldl' (flip insEdge) g es

    labNodes (Gr g) = [ (node, label)
                            | (node, (_, label, _)) <- IM.toList g ]

    -- overriding members for efficiency
    noNodes   (Gr g) = IM.size g
    nodeRange (Gr g)
        | IM.null g = (0, 0)
        | otherwise = (ix (IM.minViewWithKey g), ix (IM.maxViewWithKey g))
                  where
                    ix = fst . fst . fromJust

    labEdges (Gr g) = do (node, (_, _, s)) <- IM.toList g
                         (next, label)     <- IM.toList s
                         return (node, next, label)


instance DynGraph Gr where
    (p, v, l, s) & (Gr g)
        = let !g1 = IM.insert v (fromAdj p, l, fromAdj s) g
              !g2 = addSucc g1 v p
              !g3 = addPred g2 v s
          in
            Gr g3


matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr node (Gr g)
    = case IM.lookup node g of
        Nothing
            -> (Nothing, Gr g)

        Just (p, label, s)
            -> let !g1 = IM.delete node g
                   !p' = IM.delete node p
                   !s' = IM.delete node s
                   !g2 = clearPred g1 node (IM.keys s')
                   !g3 = clearSucc g2 node (IM.keys p')
               in
                 (Just (toAdj p', node, label, toAdj s), Gr g3)


{-# RULES
      "insNode/Physics.Falling.Collision.Graph.CollisionGraphTree"  insNode = fastInsNode
  #-}
fastInsNode :: LNode a -> Gr a b -> Gr a b
fastInsNode (v, l) (Gr g) = g' `seq` Gr g'
    where
      g' = IM.insert v (IM.empty, l, IM.empty) g


{-# RULES
      "insEdge/Physics.Falling.Collision.Graph.CollisionGraphTree"  insEdge = fastInsEdge
  #-}
fastInsEdge :: LEdge b -> Gr a b -> Gr a b
fastInsEdge (v, w, l) (Gr g) = g2 `seq` Gr g2
    where
      g1 = IM.adjust addSucc' v g
      g2 = IM.adjust addPred' w g1

      addSucc' (ps, l', ss) = (ps, l', IM.insert w l ss)
      addPred' (ps, l', ss) = (IM.insert v l ps, l', ss)

insertRemoveToMatch :: Node -> [ (Node, b) ] -> [ (Node, b) ] -> Gr a b -> Gr a b
insertRemoveToMatch node succs preds (Gr g) =
                    Gr g5
                    where
                    -- setup datas
                    (oldSuccs, nl, oldPreds)    = g IM.! node
                    newSuccs                    = IM.fromList succs
                    newPreds                    = IM.fromList preds
                    -- compute diffs
                    diff new old                = (new IM.\\ old, old IM.\\ new)
                    (toAddSucc, toRemoveSucc)   = diff newSuccs oldSuccs
                    (toAddPred, toRemovePred)   = diff newPreds oldPreds
                    -- adjust current node’s attached edges
                    finalSucc                   = toAddSucc `IM.union` (oldSuccs IM.\\ toRemoveSucc)
                    finalPred                   = toAddPred `IM.union` (oldPreds IM.\\ toRemovePred)
                    g1                          = IM.insert node (finalSucc, nl, finalPred) g
                    -- adjust other node’s edges
                    g2                          = IM.foldrWithKey (modify rmSucc')  g1 toRemovePred
                    g3                          = IM.foldrWithKey (modify rmPred')  g2 toRemoveSucc
                    g4                          = IM.foldrWithKey (modify addSucc') g3 toAddPred
                    g5                          = IM.foldrWithKey (modify addPred') g4 toAddSucc
                    -- modification helpers
                    modify method k b gr        = IM.adjust (method b) k gr
                    addSucc' b (ps, l', ss)     = (ps              , l', IM.insert node b ss)
                    addPred' b (ps, l', ss)     = (IM.insert node b ps, l', ss)
                    rmSucc' _  (ps, l', ss)     = (ps              , l', IM.delete node ss)
                    rmPred' _  (ps, l', ss)     = (IM.delete node ps  , l', ss)
                    
                    

{-# RULES
      "gmap/Physics.Falling.Collision.Graph.CollisionGraphTree"  gmap = fastGMap
  #-}
fastGMap :: forall a b c d. (Context a b -> Context c d) -> Gr a b -> Gr c d
fastGMap f (Gr g) = Gr (IM.mapWithKey f' g)
    where
      f' :: Node -> Context' a b -> Context' c d
      f' = ((fromContext . f) .) . toContext


{-# RULES
      "nmap/Physics.Falling.Collision.Graph.CollisionGraphTree"  nmap = fastNMap
  #-}
fastNMap :: forall a b c. (a -> c) -> Gr a b -> Gr c b
fastNMap f (Gr g) = Gr (IM.map f' g)
    where
      f' :: Context' a b -> Context' c b
      f' (ps, a, ss) = (ps, f a, ss)


{-# RULES
      "emap/Physics.Falling.Collision.Graph.CollisionGraphTree"  emap = fastEMap
  #-}
fastEMap :: forall a b c. (b -> c) -> Gr a b -> Gr a c
fastEMap f (Gr g) = Gr (IM.map f' g)
    where
      f' :: Context' a b -> Context' a c
      f' (ps, a, ss) = (IM.map f ps, a, IM.map f ss)


toAdj :: IntMap b -> Adj b
toAdj = map expand . IM.toList
  where
    expand (n,l) = (flip (,) n) l


fromAdj :: Adj b -> IntMap b
fromAdj = IM.fromList . map swap


toContext :: Node -> Context' a b -> Context a b
toContext v (ps, a, ss)
    = (toAdj ps, v, a, toAdj ss)


fromContext :: Context a b -> Context' a b
fromContext (ps, _, a, ss)
    = (fromAdj ps, a, fromAdj ss)


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

addSucc :: GraphRep a b -> Node -> [(b, Node)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v ((l, p) : rest) = addSucc g' v rest
    where
      g' = IM.adjust f p g
      f (ps, l', ss) = (ps, l', IM.insert v l ss)


addPred :: GraphRep a b -> Node -> [(b, Node)] -> GraphRep a b
addPred g _ []              = g
addPred g v ((l, s) : rest) = addPred g' v rest
    where
      g' = IM.adjust f s g
      f (ps, l', ss) = (IM.insert v l ps, l', ss)


clearSucc :: GraphRep a b -> Node -> [Node] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v (p:rest) = clearSucc g' v rest
    where
      g' = IM.adjust f p g
      f (ps, l, ss) = (ps, l, IM.delete v ss)


clearPred :: GraphRep a b -> Node -> [Node] -> GraphRep a b
clearPred g _ []       = g
clearPred g v (s:rest) = clearPred g' v rest
    where
      g' = IM.adjust f s g
      f (ps, l, ss) = (IM.delete v ps, l, ss)
