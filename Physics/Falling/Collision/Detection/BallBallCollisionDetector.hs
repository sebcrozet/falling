module Physics.Falling.Collision.BallBallCollisionDetector
where

import Data.Vect.Double.Base
import Physics.Falling.Shape.ExtraTransform
import Physics.Falling.Shape.Ball
import Physics.Falling.Collision.Collision

-- generic ballball collision detection algorithm
collideBallBall :: (Position m v, UnitVector v n) =>
                   Ball -> Ball -> m -> m -> Maybe (GeometricCollisionDescr v n)
collideBallBall (Ball r1) (Ball r2) transform1 transform2 =
                                            if sqDist <= (r1 + r2) ** 2.0 then
                                              Just $ GeometricCollisionDescr
                                                     collisionCenter
                                                     collisionNormal
                                                     depth
                                            else
                                              Nothing

                                            where
                                            center1         = position transform1
                                            center2         = position transform2
                                            deltaPos        = center1 &- center2
                                            collisionNormal = mkNormal deltaPos
                                            collisionCenter = center1 &+ center2 &* 0.5
                                            sqDist          = normsqr deltaPos
                                            depth           = -(sqrt sqDist - r1 - r2)
