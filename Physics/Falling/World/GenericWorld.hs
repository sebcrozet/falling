module Physics.Falling.World.GenericWorld
(
World
, mkWorld
, activeBodies
, collisionGraph
, addRigidBody
, addRigidBodies
, removeRigidBody
, removeRigidBodies
, step
)
where

import Data.List
import qualified Data.Map as M
import Physics.Falling.Collision.Graph.CollisionGraph hiding (collisions)
import Physics.Falling.Collision.Detection.BroadPhase hiding (addBody, removeBody, update)
import qualified Physics.Falling.Collision.Detection.BroadPhase as BP
import Physics.Falling.Collision.Detection.NarrowPhase
import Physics.Falling.Integrator.Integrable
import Physics.Falling.Identification.Identifiable
import Physics.Falling.Identification.IndexGenerator

data (Identifiable rb idt, IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
      World rb bf nf cm ig idt
      = World
        {
          broadPhase                     :: bf
          , collisionDetectionDispatcher :: rb -> rb -> nf
          , collisionGraph               :: CollisionGraph rb nf
          , constraintSolver             :: Double -> [ (Int, rb) ] -> [ cm ] -> ( [ (Int, rb) ], [ cm ] )
          , islandNodeFilter             :: CollisionLink nf -> Bool
          , islandEdgeFilter             :: CollisionLink nf -> Bool
          , activeBodyFilter             :: rb -> Bool
          , activeBodies                 :: [rb]
          , nodeId2GraphIndex            :: M.Map idt Int
          , graphIndex2Node              :: M.Map Int rb -- FIXME: use an IntMap ?
          , node2GraphIndexGenerator     :: ig
          -- lcpSolver
        }

mkWorld :: (Identifiable rb idt, IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
           bf ->
           (rb -> rb -> nf) ->
           (Double -> [ (Int, rb) ] -> [ cm ] -> ( [ (Int, rb) ], [ cm ])) ->
           (CollisionLink nf -> Bool) ->
           (CollisionLink nf -> Bool) ->
           (rb -> Bool) ->
           ig ->
           World rb bf nf cm ig idt
mkWorld initBroadPhase
        initCollisionDetectionDispatcher
        initConstraintSolver
        initIslandNodeFilter
        initIslandEdgeFilter
        initActiveBodyFilter
        initIndexGenerator
        = World
          {
            broadPhase                     = initBroadPhase
            , collisionDetectionDispatcher = initCollisionDetectionDispatcher
            , collisionGraph               = emptyCollisionGraph 
            , constraintSolver             = initConstraintSolver
            , islandNodeFilter             = initIslandNodeFilter
            , islandEdgeFilter             = initIslandEdgeFilter
            , activeBodyFilter             = initActiveBodyFilter
            , activeBodies                 = []
            , nodeId2GraphIndex            = M.empty
            , graphIndex2Node              = M.empty
            , node2GraphIndexGenerator     = initIndexGenerator
          }

addRigidBody :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                rb -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
addRigidBody b world = world {
                         broadPhase                 = BP.addBody b $ broadPhase world
                         , collisionGraph           = insertBody (bodyId, b) $ collisionGraph world
                         , activeBodies             = newActiveBodies
                         , nodeId2GraphIndex        = M.insert (identifier b) bodyId $ nodeId2GraphIndex world
                         , graphIndex2Node          = M.insert bodyId b $ graphIndex2Node world
                         , node2GraphIndexGenerator = updatedGenerator
                       }
                       where
                       currentActiveBodies        = activeBodies world
                       (updatedGenerator, bodyId) = generate b $ node2GraphIndexGenerator world
                       newActiveBodies            = if activeBodyFilter world $ b then
                                                      b:currentActiveBodies
                                                    else
                                                      currentActiveBodies

addRigidBodies :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                  [rb] -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
addRigidBodies bs world = foldr addRigidBody world bs

removeRigidBody :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                   rb -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
removeRigidBody b world = world {
                            broadPhase                 = BP.removeBody b $ broadPhase world
                            , collisionGraph           = removeBody bodyGraphIndex $ collisionGraph world
                            , activeBodies             = deleteBy (\ba bb -> identifier ba == identifier bb)
                                                                  b
                                                                  (activeBodies world)
                            , nodeId2GraphIndex        = M.delete bodyIdentifier $ nodeId2GraphIndex world
                            , graphIndex2Node          = M.delete bodyGraphIndex $ graphIndex2Node world
                            , node2GraphIndexGenerator = recycle b bodyGraphIndex currentGenerator
                          }
                          where
                          currentGenerator = node2GraphIndexGenerator world
                          bodyIdentifier   = identifier b
                          bodyGraphIndex   = nodeId2GraphIndex world M.! bodyIdentifier

removeRigidBodies :: (Identifiable rb idt, IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                     [rb] -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
removeRigidBodies bs world = foldr removeRigidBody world bs

step :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
        Double -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
step dt world = newWorld
                where
                dispatcher = collisionDetectionDispatcher world
                n2g        = nodeId2GraphIndex world
                ef         = islandEdgeFilter  world
                nf         = islandNodeFilter  world
                cs         = constraintSolver  world

                -- integrate
                forwardDynamicsBodies = map (integratePosition dt . integrateVelocity dt) $ activeBodies world
                fdBodiesWithIndex     = map (\b -> (n2g M.! identifier b, b)) forwardDynamicsBodies

                -- execute broad phase
                (newColls, collsLost, newBroadPhase) = BP.update forwardDynamicsBodies $ broadPhase world
                newCollisions = map (\(r1, r2) -> (n2g M.! (identifier r1), -- FIXME: ensure that idr1 < idr2
                                                   n2g M.! (identifier r2),
                                                   dispatcher r1 r2))
                                    newColls
                lostCollisions = map (\(r1, r2) -> (n2g M.! (identifier r1), -- FIXME: ensure that idr1 < idr2
                                                    n2g M.! (identifier r2)))
                                    collsLost

                -- insert new collisions ...
                afterRemoveCollisionGraph  = removeCollisions lostCollisions $ collisionGraph world
                -- ... get all active collisions and update narrow phases ...
                colls                      = collisionsWithBodies' afterRemoveCollisionGraph fdBodiesWithIndex
                bg                         = insertBodies fdBodiesWithIndex afterRemoveCollisionGraph 
                collsWithNewColls          = colls ++ newCollisions
                updatedColls               = map (\(i1, i2, np) -> (i1, i2, update (body' bg i1) (body' bg i2) np))
                                                 collsWithNewColls
                -- ... then update collision graph updated narrow phases ...
                afterUpdateCollisionGraph  = insertCollisions updatedColls bg
                -- ... and finally get all active islands
                islands = collisionGroupsAndBodies' afterUpdateCollisionGraph fdBodiesWithIndex nf ef

                -- solve islands
                third (_, _, v) = v       
                solvedContacts = map (\(bs, nfs) -> cs dt bs $ map (collisions.third) nfs) islands
                solvedBodies   = concatMap fst solvedContacts

                -- update the world
                newWorld = world {
                             activeBodies     = map snd solvedBodies
                             , broadPhase     = newBroadPhase
                             , collisionGraph = afterUpdateCollisionGraph
                           }
