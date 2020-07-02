module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity
  , applyGravity
  , bounce
  ) where

import Graphics.Gloss
import Geo

type Velocity = Vector2D

data Particle = Particle
  { position :: Point2D
  , velocity :: Velocity
  , mass :: Double
  } deriving Show

data ParticleSys = ParticleSys [Particle] deriving Show

gravity :: (Double, Double)
gravity = (0, -100.0)

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

frictionY :: Velocity -> Velocity
frictionY (V vx vy) =
    V ( if (abs vx)> 0.5 then vx*0.999 else 0)
      ( if (abs vy)> 0.5 then vy*friction else 0)
  where
    friction = 0.85


-- dt: seconds , mass, currentVelocity -> newVelocity
applyGravity :: Double -> Velocity -> Velocity
applyGravity dt (V vx vy) = V (vx + dvx) (vy + dvy)
  where
    (dvx, dvy) = (fst gravity * dt, snd gravity * dt)

-- Bounce against a horizontal wall in 0,0
-- First parameter is the radius of the sphere bouncing
-- Second parameter is the couple Velocity and Position of the sphere bouncing
-- To use this function transform incoming (Velocity, Point) to
-- the wall reference frame with the Zero on the wall,
-- the X axis along the wall
-- and the Y axis perpendicular and coming out of the wall
bounce :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounce r ((V vx vy), (P px py)) =
    if vy<0 && pby < 0 then
      ( frictionY (V vx (-vy))  -- New velocity
      , P px (py - 2 * pby)  -- New position
      )
    else
      ( (V vx vy), (P px py))
  where
    pby = py - r
