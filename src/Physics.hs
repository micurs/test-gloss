module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity
  , Obstruction(..)
  , isWall
  , isCircle
  , gravityAcceleration
  , applyGravity
  , bounce
  , addParticle2Sys
  , movePos
  , distance
  , distanceSq
  , collideParticles
  , geoMapParticle
  , alignedP2CBounce
  , module Physics.Circle
  , module Physics.Wall
  ) where

import Graphics.Gloss
import Geo
import Physics.Wall
import Physics.Circle

type Velocity = Vector2D
type Acceleration = Vector2D

data Obstruction = C Circle | W Wall deriving Show

data Particle = Particle
  { position :: Point2D
  , velocity :: Velocity
  , acceleration :: Acceleration
  , mass :: Double
  , life :: Int
  } deriving (Show, Eq)

data ParticleSys = ParticleSys [Particle] deriving Show

isWall :: Obstruction -> Bool
isWall (C _) = False
isWall (W _) = True

isCircle :: Obstruction -> Bool
isCircle (C _) = True
isCircle (W _) = False

addParticle2Sys :: ParticleSys -> Particle -> ParticleSys
addParticle2Sys (ParticleSys pl) p = ParticleSys (p : pl)

gravityAcceleration :: Vector2D
gravityAcceleration = V 0 (-980.0)

movePos :: Double -> Velocity -> Point2D -> Point2D
movePos tm v p = p .-> (tm .> v)

distance :: Particle -> Particle -> Double
distance p1 p2 = vLength v
  where
    v = (position p1) `minus` (position p2) :: Vector2D

distanceSq:: Particle -> Particle -> Double
distanceSq p1 p2 = vLengthSq v
  where
    v = (position p1) `minus` (position p2) :: Vector2D

particleFriction :: Velocity -> Velocity
particleFriction (V vx vy) =
    V ( if (abs vx)> 0.01 then vx*friction else 0)
      ( if (abs vy)> 0.01 then vy*friction else 0)
  where
    friction = 1.0

frictionY :: Double -> Velocity -> Velocity
frictionY f (V vx vy) =
    V ( if (abs vx)> 0.1 then vx*f else 0)
      ( if (abs vy)> 0.1 then vy*f else 0)

-- dt: seconds , mass, currentVelocity -> newVelocity
applyGravity :: Double -> Velocity -> Velocity
applyGravity tm v = (tm .> gravityAcceleration) `add` v

-- isBehindWall :: Double -> Point2D -> Bool
-- isBehindWall (P _ py) = py < 0

isHeadingToWall :: Velocity -> Bool
isHeadingToWall (V _ vy) = vy < 0


-- Compute the X value of intersection between the 2 points
collideWithin :: Double -> Double -> Point2D -> Point2D -> Bool
collideWithin d r p1 p2
  | p1y >= 0 && p2y <= r =
      let
        s = - (p2y / (p1y - p2y))
        c = (p2x + s * (p1x - p2x))
      in
        c < d && c > (-d)
  | otherwise = False
  where
    (P p1x p1y) = p1
    (P p2x p2y) = p2

-- Bounce against a horizontal wall position in 0,0 and facing UP
-- the X axis along the wall and and the Y axis perpendicular and coming out of the wall
bounce ::  Wall                             -- The wall to bounce against
        -> Double                           -- The radius of the circle bouncing
        -> (Velocity, Point2D, Point2D)     -- Velocity, Original Position and New (potential) Position of the circle
        -> (Velocity, Point2D, Point2D)     -- New Velocity, original position (unchanged) and New Position after the bounce!
bounce wall r (vel, oldP, newP)
  | or [ vy >= 0.0
        , newPTy<0 && oldPTy<0
        , newPx>size && oldPx>size
        , newPx< (-size) && oldPx<(-size)
        , newPBy > 0
        ]  = (vel, oldP, newP)
  | otherwise = let
        (newVel, newPos) =
          if collideWithin size r oldP newP then
            ( frictionY f (V vx (-vy))
            , P newPx (newPy - 2 * newPBy))
          else
            (vel, newP)
        in (newVel, oldP, newPos)
  where
    size = (wallDim wall) / 2
    (V vx vy) = vel
    (P oldPx oldPy) = oldP
    (P newPx newPy) = newP
    newPTy = newPy + r
    oldPTy = oldPy + r
    newPBy = newPy - r
    oldPBy = oldPy - r
    f = wallFriction wall

-- collX = oldPBy - (oldPx - newPx) / (oldPBy - newPBy) * oldPBy -- Collision point
-- | vy <=0 && vy >= -1 && newPBy < 0 = ( (V vx 0), oldP, (P oldPx r ) )

geoMapParticle :: Frame -> Particle -> Particle
geoMapParticle f p = p { position = geoMap f $ position p
                       , velocity = geoMap f $ velocity p
                       }

geoMapCoupleParticle :: Frame -> (Particle, Particle) -> (Particle, Particle)
geoMapCoupleParticle f (p1, p2) = (geoMapParticle f p1, geoMapParticle f p2)

alignedP2CBounce :: (Particle, Circle) -> Particle
alignedP2CBounce (p,c)
  | dc > md = p
  | otherwise =
      let
        d = x (center c) - md
        (V vx vy) = velocity p
      in p { velocity = V (-vx) vy, position = P d 0 }
  where
    dc = x (center c) - x (position p)
    md =  (radius c) + (mass p / 2)


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
    v12 = (position p2) `minus` (position p1)
    pc = (position p1) .-> (0.5 .> v12)
    frameXaxe = (unitVector2D v12)
    toLocalFrame = frame pc frameXaxe
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

