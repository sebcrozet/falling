module Physics.Falling.World.GenericWorld
(
World
, mkWorld
, activeBodies
, collisionGraph
, rigidBodies
, addRigidBody
, addRigidBodies
, updateRigidBody
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
          , -- | The world collision graph. It contains any information about which object is in
            -- contact with which other object. It also contains informations about contact
            -- manifolds.
            collisionGraph               :: CollisionGraph rb nf
          , constraintSolver             :: Double -> [ (Int, rb) ] -> [ cm ] -> ( [ (Int, rb) ], [ cm ] )
          , islandNodeFilter             :: CollisionLink nf -> Bool
          , islandEdgeFilter             :: CollisionLink nf -> Bool
          , activeBodyFilter             :: rb -> Bool
          , -- | Active bodies handled by the physics world. An active body is assumed to be a body
            -- which can move (eg. not the ground) and which is quite moving at the moment (eg. it
            -- is not asleep). Use this to get the list of bodies which might have been moved by
            -- the physics engine during the last update.
            activeBodies                 :: [rb]
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

-- | The list of all bodies present on a physics world.
rigidBodies ::(Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
              World rb bf nf cm ig idt -> [ rb ]
rigidBodies = (map snd).bodies.collisionGraph

-- | Adds a body to a physics world.
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

-- | Adds several bodies to a physics world.
addRigidBodies :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                  [rb] -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
addRigidBodies bs world = foldr addRigidBody world bs

-- | Notifies the world that the state of has been changed from the outside of the engine.
updateRigidBody :: (Identifiable rb idt , IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                   rb -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
updateRigidBody b world = world { -- FIXME: wont work for static bodies and inactive bodies
                            activeBodies = b : (deleteBy (\b1 b2 -> identifier b1 == identifier b2) b $ activeBodies world)
                          }

-- | Removes a body from the physics world.
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

-- | Removes several bodies from the physics world.
removeRigidBodies :: (Identifiable rb idt, IndexGenerator ig rb, Integrable rb, BroadPhase bf rb , NarrowPhase nf rb cm) =>
                     [rb] -> World rb bf nf cm ig idt -> World rb bf nf cm ig idt
removeRigidBodies bs world = foldr removeRigidBody world bs

-- | Updates the world assuming a user-defined time step occured since the last update. It is
-- recommended to keep the time-step small and constant over updates. A typical and recommeded
-- time-step value for video games is 0.016 (for 60Hz simulation).
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
                forwardDynamicsBodies = map (integrateVelocity dt) $ activeBodies world
                fdBodiesWithIndex     = map (\b -> (n2g M.! identifier b, b)) forwardDynamicsBodies

                -- execute broad phase
                (newColls, collsLost, newBroadPhase) = BP.update forwardDynamicsBodies $ broadPhase world
                newCollisions = map (\(r1, r2) -> let idr1 = n2g M.! (identifier r1) in
                                                  let idr2 = n2g M.! (identifier r2) in
                                                  if idr1 < idr2 then
                                                    (idr1, idr2, dispatcher r1 r2)
                                                  else
                                                    (idr2, idr1, dispatcher r2 r1)
                                    )
                                    newColls
                lostCollisions = map (\(r1, r2) -> let idr1 = n2g M.! (identifier r1) in
                                                   let idr2 = n2g M.! (identifier r2) in
                                                   if idr1 < idr2 then
                                                     (idr1, idr2)
                                                   else
                                                     (idr2, idr1)
                                     )
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
                third (_, _, v)    = v       
                solvedContacts     = map (\(bs, nfs) -> cs dt bs $ map (collisions.third) nfs) islands
                solvedBodies       = map snd $ concatMap fst solvedContacts
                repositionedBodies = map (integratePosition dt) $ solvedBodies

                -- update the world
                newWorld = world {
                             activeBodies     = repositionedBodies
                             , broadPhase     = newBroadPhase
                             , collisionGraph = afterUpdateCollisionGraph
                           }
