module Physics.Falling.Collision.Detection.BallBallCollisionDetector
(
collideBallBall
)
where

import Physics.Falling.Math.Transform
import Physics.Falling.Shape.Ball
import Physics.Falling.Collision.Collision

collideBallBall :: (Vector v, UnitVector v n) =>
                   Ball v -> Ball v -> Maybe (PartialCollisionDescr v n)
collideBallBall (Ball c1 r1) (Ball c2 r2) =
                if sqDist <= (r1 + r2) ** 2.0 then
                  Just $ (cp1, cp2, collisionNormal, depth)
                else
                  Nothing

                where
                deltaPos        = c2 &- c1
                collisionNormal = mkNormal deltaPos
                nvect           = fromNormal collisionNormal
                cp1             = c1 &+ r1 *& nvect
                cp2             = c2 &- r2 *& nvect
                sqDist          = normsqr deltaPos
                depth           = r1 + r2 - sqrt sqDist 
