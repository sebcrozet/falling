module Physics.Falling.Collision.Detection.BallBallCollisionDetector
(
collideBallBall
)
where

import Data.Vect.Double.Base hiding(translation)
import Physics.Falling.Math.Transform
import Physics.Falling.Shape.Ball
import Physics.Falling.Collision.Collision

-- generic ballball collision detection algorithm
collideBallBall :: (Translation m v, UnitVector v n) =>
                   Ball -> Ball -> m -> m -> Maybe (CollisionDescr v n)
collideBallBall (Ball r1) (Ball r2) transform1 transform2 =
                                            if sqDist <= (r1 + r2) ** 2.0 then
                                              Just $ CollisionDescr collisionCenter
                                                                    collisionNormal
                                                                    depth
                                            else
                                              Nothing

                                            where
                                            center1         = translation transform1
                                            center2         = translation transform2
                                            deltaPos        = center2 &- center1
                                            collisionNormal = mkNormal deltaPos
                                            nvect           = fromNormal collisionNormal
                                            collisionCenter = (nvect &* (r1 - r2) &+ center1 &+ center2) &* 0.5
                                            sqDist          = normsqr deltaPos
                                            depth           = r1 + r2 - sqrt sqDist 
