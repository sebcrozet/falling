{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}

module Physics.Falling.Collision.Graph.CollisionGraph
(
CollisionGraph
, Body
, BodyId
, Collision
, CollisionId
, CollisionLink
, emptyCollisionGraph
, extractBody
, extractCollision
, insertBody
, insertCollision
, removeBody
, removeCollision
, insertBodies
, insertCollisions
, removeBodies
, removeBodies'
, removeCollisions
, removeCollisions'
, modifyBody
, modifyCollision
, modifyCollisions
, body
, body'
, bodies
, bodies'
, collisions
, collisions'
, collisionGroupsAndBodies
, collisionGroupsAndBodies'
, collisionsWithBodies
, collisionsWithBodies'
, extractIslands
, extractIslands'
)
where

import Control.Exception.Base
import Data.Graph.Inductive.Graph
import Physics.Falling.Collision.Graph.CollisionGraphTree -- Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Tree
import Data.List(foldl')

type CollisionGraph b c = Gr b c
type BodyId             = Int
type CollisionId        = Int
type Body           b   = LNode b     -- ~ (Int, b)
type Collision      c   = LEdge c     -- ~ (Int, Int, c)
type CollisionLink  c   = (c, BodyId) -- ~ (c, Int)

-- initialization
emptyCollisionGraph :: CollisionGraph b c
emptyCollisionGraph = empty

-- note queries
extractBody :: Body b -> b
extractBody = snd

extractCollision :: Collision c -> c
extractCollision (_, _, coll) = coll

-- insertion
insertBody :: Body b -> CollisionGraph b c -> CollisionGraph b c
insertBody = insNode

insertCollision :: Collision c -> CollisionGraph b c -> CollisionGraph b c
insertCollision coll = assert (_isCollisionValid' coll)
                       $ insEdge coll

insertBodies :: [Body b] -> CollisionGraph b c -> CollisionGraph b c
insertBodies ns g = foldl' (flip fastInsNode) g ns -- insNodes

insertCollisions :: [Collision c] -> CollisionGraph b c -> CollisionGraph b c
insertCollisions colls g = assert (all _isCollisionValid' colls)
                           $ foldl' (flip fastInsEdge) g colls

-- removal
removeBody :: BodyId -> CollisionGraph b c -> CollisionGraph b c
removeBody = delNode

removeBody' :: Body b -> CollisionGraph b c -> CollisionGraph b c
removeBody' (idb, _) = removeBody idb

removeCollision :: (BodyId, BodyId) -> CollisionGraph b c -> CollisionGraph b c
removeCollision coll = assert (_isCollisionValid coll)
                       $ delEdge coll

removeCollision' :: Collision c -> CollisionGraph b c -> CollisionGraph b c
removeCollision' (id1, id2, _) = removeCollision (id1, id2)

removeBodies :: [BodyId] -> CollisionGraph b c -> CollisionGraph b c
removeBodies = delNodes

removeBodies' :: [Body b] -> CollisionGraph b c -> CollisionGraph b c
removeBodies' = removeBodies . map fst

removeCollisions :: [(BodyId, BodyId)] -> CollisionGraph b c -> CollisionGraph b c
removeCollisions colls = assert (all _isCollisionValid colls)
                         $ delEdges colls

removeCollisions' :: [Collision c] -> CollisionGraph b c -> CollisionGraph b c
removeCollisions' = removeCollisions . map (\(a, b, _) -> (a, b))

-- modification
modifyCollision :: Collision c -> CollisionGraph b c -> CollisionGraph b c
modifyCollision new = insertCollision new . removeCollision' new

modifyBody :: Body b -> CollisionGraph b c -> CollisionGraph b c
modifyBody new g = insertCollisions oldCollisions gWithNewBody
                   where
                   oldCollisions = collisionsWithBody' g new
                   gWithNewBody  = insertBody new $ removeBody' new g

modifyCollisions :: [Collision c] -> CollisionGraph b c -> CollisionGraph  b c
modifyCollisions news = insertCollisions news . removeCollisions' news

-- FIXME: implement efficient modifyBodies

-- queries
body :: CollisionGraph b c -> BodyId -> b
body g i = let (Just l) = lab g i in l

body' :: CollisionGraph b c -> BodyId -> Body b
body' g i = (i, body g i)

bodies :: CollisionGraph b c -> [Body b]
bodies = labNodes

bodies' :: CollisionGraph b c -> [b]
bodies' g = map snd (bodies g)

collisions :: CollisionGraph b c -> [Collision c]
collisions = labEdges

collisions' :: CollisionGraph b c -> [c]
collisions' = map third . collisions
              where
              third (_, _, c) = c

collisionsWithBody :: CollisionGraph b c -> BodyId -> [Collision c]
collisionsWithBody g b = let (preds, _, _, succs) = context g b in
                         fmap formatPredResult preds ++ fmap formatSuccResult succs
                         where
                         formatPredResult (lbl, nid) = (nid, b, lbl)
                         formatSuccResult (lbl, nid) = (b, nid, lbl)

collisionsWithBody' :: CollisionGraph b c -> Body b -> [Collision c]
collisionsWithBody' g b = collisionsWithBody g $ fst b

collisionsWithBodies :: CollisionGraph b c -> [BodyId] -> [Collision c]
collisionsWithBodies g bs = extractIslands g bs acceptNodeTraversalMethod valuationMethod flattenMethod
                            where
                            acceptNodeTraversalMethod _ = False
                            flattenMethod = concat . map concat
                            valuationMethod (preds, nid, _, succs) = (fmap (_formatSuccResult nid) succs) ++
                                                                     (fmap (_formatPredResult nid) preds)

collisionsWithBodies' :: CollisionGraph b c -> [Body b] -> [Collision c]
collisionsWithBodies' g bs = collisionsWithBodies g (map fst bs)

collisionGroupsAndBodies  :: CollisionGraph b c ->
                             [BodyId] ->
                             (CollisionLink c -> Bool) ->
                             (CollisionLink c -> Bool) ->
                             [([Body b], [Collision c])]
collisionGroupsAndBodies g
                         startNodes
                         acceptNodeTraversal
                         acceptEdgeValue
                         = extractIslands g startNodes acceptNodeTraversal valuationMethod flattenMethod
                         where
                         flattenMethod = map (\(l1, l2) -> (l1, concat l2)) . map unzip
                         valuationMethod (preds, nid, n, succs) =
                            ((nid, n),
                             (fmap (_formatSuccResult nid) $ filter acceptEdgeValue succs) ++
                             (fmap (_formatPredResult nid) $ filter acceptEdgeValue preds))

collisionGroupsAndBodies'  :: CollisionGraph b c ->
                             [Body b] ->
                             (CollisionLink c -> Bool) ->
                             (CollisionLink c -> Bool) ->
                             [([Body b], [Collision c])]
collisionGroupsAndBodies' g startNodes = collisionGroupsAndBodies g (map fst startNodes)

extractIslands :: CollisionGraph b c ->
                  [BodyId] ->
                  (CollisionLink c -> Bool) ->
                  (Context b c -> v) ->
                  ([[v]] -> f) ->
                  f
extractIslands g
               startNodes
               acceptNodeTraversal
               valuationMethod
               flattenMethod
               = islands
               where
               islands = flattenMethod $ fmap flatten forest
               forest = xdffWith traversalMethod valuationMethod startNodes g
               traversalMethod (preds, _, _, succs) = map snd $ filter acceptNodeTraversal (preds ++ succs)

extractIslands' :: CollisionGraph b c ->
                   [Body b] ->
                   (CollisionLink c -> Bool) ->
                   (Context b c -> v) ->
                   ([[v]] -> f) ->
                   f
extractIslands' g startNodes = extractIslands g (map fst startNodes)

-- non-exported functions
_isCollisionValid :: (BodyId, BodyId) -> Bool
_isCollisionValid (id1, id2) = id1 < id2

_isCollisionValid' :: Collision c -> Bool
_isCollisionValid' (id1, id2, _) = _isCollisionValid (id1, id2)

_formatPredResult :: a -> (b, c) -> (c, a, b)
_formatPredResult nid (lbl, oid) = (oid, nid, lbl)

_formatSuccResult :: a -> (b, c) -> (a, c, b)
_formatSuccResult nid (lbl, oid) = (nid, oid, lbl)
