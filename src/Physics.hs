module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity
  , applyGravity
  , bounce
  , addParticle2Sys
  , module Physics.Wall
  ) where

import Graphics.Gloss
import Geo
import Physics.Wall

type Velocity = Vector2D

data Particle = Particle
  { position :: Point2D
  , velocity :: Velocity
  , mass :: Double
  } deriving Show

data ParticleSys = ParticleSys [Particle] deriving Show

addParticle2Sys :: ParticleSys -> Particle -> ParticleSys
addParticle2Sys (ParticleSys pl) p = ParticleSys (p : pl)

gravity :: (Double, Double)
gravity = (0, -98.0)

friction :: Velocity -> Velocity
friction (V vx vy) =
    V ( if (abs vx)> 0.5 then vx*friction else 0)
      ( if (abs vy)> 0.5 then vy*friction else 0)
  where
    friction = 0.6

frictionX :: Velocity -> Velocity
frictionX (V vx vy) =
    V ( if (abs vx)> 0.5 then vx*friction else 0)
      ( if (abs vy)> 0.5 then vy*0.99 else 0)
  where
    friction = 0.6

frictionY :: Double -> Velocity -> Velocity
frictionY f (V vx vy) =
    V ( if (abs vx)> 0.1 then vx*0.99 else 0)
      ( if (abs vy)> 0.5 then vy*friction else 0)
  where
    friction = f


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
    | or [ vy >= 0 || (newPBy<0 && oldPBy<0)
         , newPx > size && oldPx > size
         , newPx < (-size) && oldPx < (-size)
         , newPBy > 0
         ]  = (vel, oldP, newP)
    | otherwise = ( frictionY f (V vx (-vy))  -- New velocity
                  , oldP
                  , P newPx (newPy - 2 * newPBy)  -- New position
                  )
  where
    size = (wallDim wall) / 2 + r
    (V vx vy) = vel
    (P oldPx oldPy) = oldP
    (P newPx newPy) = newP
    newPBy = newPy - r
    oldPBy = oldPy - r
    f = wallFriction wall

