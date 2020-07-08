module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity
  , applyGravity
  , bounce
  , addParticle2Sys
  , movePos
  , distance
  , distanceSq
  , collideParticles
  , module Physics.Wall
  ) where

import Graphics.Gloss
import Geo
import Physics.Wall

type Velocity = Vector2D

type Acceleration = Vector2D

data Particle = Particle
  { position :: Point2D
  , velocity :: Velocity
  , acceleration :: Acceleration
  , mass :: Double
  , life :: Int
  } deriving (Show, Eq)

data ParticleSys = ParticleSys [Particle] deriving Show

addParticle2Sys :: ParticleSys -> Particle -> ParticleSys
addParticle2Sys (ParticleSys pl) p = ParticleSys (p : pl)

gravity :: (Double, Double)
gravity = (0, -150.0)

movePos :: Double -> Velocity -> Point2D -> Point2D
movePos tm (V vx vy) (P px py) =
  P (px + vx * tm)  (py + vy * tm)

distance :: Particle -> Particle -> Double
distance p1 p2 = vectorLength $ vectorFromPoints (position p1) (position p2)

distanceSq:: Particle -> Particle -> Double
distanceSq p1 p2 = vectorLengthSq $ vectorFromPoints (position p1) (position p2)

particleFriction :: Velocity -> Velocity
particleFriction (V vx vy) =
    V ( if (abs vx)> 0.1 then vx*friction else 0)
      ( if (abs vy)> 0.1 then vy*friction else 0)
  where
    friction = 0.9

frictionY :: Double -> Velocity -> Velocity
frictionY f (V vx vy) =
    V ( if (abs vx)> 0.1 then vx*f else 0)
      ( if (abs vy)> 0.1 then vy*f else 0)

-- dt: seconds , mass, currentVelocity -> newVelocity
applyGravity :: Double -> Velocity -> Velocity
applyGravity dt (V vx vy) = V (vx + dvx) (vy + dvy)
  where
    (dvx, dvy) = (fst gravity * dt, snd gravity * dt)


-- isBehindWall :: Double -> Point2D -> Bool
-- isBehindWall (P _ py) = py < 0

isHeadingToWall :: Velocity -> Bool
isHeadingToWall (V _ vy) = vy < 0

-- Bounce against a horizontal wall in 0,0
-- First parameter is the radius of the sphere bouncing
-- Second parameter is the couple Velocity and Position of the sphere bouncing
-- To use this function transform incoming (Velocity, Point) to
-- the wall reference frame with the Zero on the wall,
-- the X axis along the wall
-- and the Y axis perpendicular and coming out of the wall
bounce :: Wall -> Double -> (Velocity, Point2D, Point2D) -> (Velocity, Point2D, Point2D)
bounce wall r (vel, oldP, newP)
    | or [ vy >= 0.0
         , newPBy<0 && oldPBy<0
         , newPx>size && oldPx>size
         , newPx< (-size) && oldPx<(-size)
        --  , (collX >= size || collX <= size) && newPBy < 0.0 && oldPBy > 0.0
         , newPBy > 0
         ]  = (vel, oldP, newP)
    -- | vy <=0 && vy >= -1 && newPBy < 0 = ( (V vx 0), oldP, (P oldPx r ) )
    | otherwise = ( frictionY f (V vx (-vy))  -- New velocity
                  , oldP
                  , P newPx (newPy - 2 * newPBy)  -- New position
                  )
  where
    size = (wallDim wall) / 2 + r
    (V vx vy) = vel
    (P oldPx oldPy) = oldP
    (P newPx newPy) = newP
    newPTy = newPy + r
    oldPTy = oldPy + r
    newPBy = newPy - r
    oldPBy = oldPy - r
    -- collX = oldPBy - (oldPx - newPx) / (oldPBy - newPBy) * oldPBy -- Collision point
    f = wallFriction wall


geoMapParticle :: Frame -> Particle -> Particle
geoMapParticle f p = p { position = geoMap f $ position p
                       , velocity = geoMap f $ velocity p
                       }

geoMapCoupleParticle :: Frame -> (Particle, Particle) -> (Particle, Particle)
geoMapCoupleParticle f (p1, p2) = (geoMapParticle f p1, geoMapParticle f p2)

-- Computes the collision between two particles defined
-- in a local reference frame centered half way between
-- the 2 centers and with x-axes aligned along the
-- direction connecting the 2 centers.
{-
  elastic collision =>
   - v1 + v1' = v2 + v2'                    (1)
   - m1*v1 + m2*v2 = m1*v1' + m2*v2'        (2)
-}
alignedP2PBounce :: (Particle, Particle) -> (Particle, Particle)
alignedP2PBounce (p1, p2) =
    ( p1 { velocity = particleFriction (V v1x'  (y $ velocity p1))
         , position = P (-d) 0
         }
    , p2 { velocity = particleFriction (V v2x'  (y $ velocity p2))
         ,  position = P d 0
         }
    )
  where
    r1 = mass p1 / 2
    r2 = mass p2 / 2
    d = (r1 + r2) / 2
    (V v1x v1y) = velocity p1
    (V v2x v2y) = velocity p2
    tv = v1x + v2x
    m1 = mass p1
    m2 = mass p2
    tm = m1 + m2                              -- total mass of the 2 balls
    -- tM = m1 * v1x + m2 * v2x                  -- total momentum of the 2 balls system
    -- v2x' = (tM + m1 * v1x - m1 * v2x) / tm    -- solve (2) for v2x'
    -- v1x' = v2x - v1x + v2x'                   -- solve (1) for v1x'
    v1x' = v1x*(m1-m2)/tm + v2x*(2*m2)/tm
    v2x' = v1x*(2*m1)/tm + v2x*(m2-m1)/tm


p2pBounce :: Particle -> Particle -> (Particle, Particle)
p2pBounce p1 p2 =
    geoMapCoupleParticle toGlobalFrame $ alignedP2PBounce lp12
  where
    v12 = vectorFromPoints (position p2) (position p1)
    pc = pointPlusVector (position p1) (scalarDot 0.5 v12)
    frameXaxe = (unitVector v12)
    toLocalFrame = frame pc frameXaxe $ orthoUnitVector frameXaxe
    toGlobalFrame = invert toLocalFrame
    lp12 = geoMapCoupleParticle toLocalFrame (p1, p2)


bounceIfColliding :: Particle -> Particle -> (Particle, Particle)
bounceIfColliding p1 p2
  | d >= (cd*cd) = (p1, p2)
  | otherwise   = p2pBounce p1 p2
  where
    d = abs $ distanceSq p1 p2
    cd = (mass p1 + mass p2) / 2

-- Collide a particle against a list of other particles.
-- Return the state of the first particle (after the collision)
-- and the state of all the remaining particles (also after the collisions)
collideParticle :: Particle -> [Particle] -> (Particle, [Particle])
collideParticle p1 [p2] = (p1c,[p2c])
  where
    (p1c, p2c) = bounceIfColliding p1 p2

collideParticle p1 (p2:pl) =
    (p1f, p2c : plc)
  where
    (p1c, p2c) = bounceIfColliding p1 p2
    (p1f, plc) = collideParticle p1c pl


-- Collides the particle in the given list.
-- Returns the list of particles modified with the collisions
collideParticles :: [Particle] -> [Particle]
collideParticles [] = []
collideParticles [p] = [p]
collideParticles (p1:pl) = p1c : plcf
  where
    (p1c,plc) = collideParticle p1 pl
    plcf = collideParticles plc

