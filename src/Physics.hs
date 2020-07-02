module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity
  , applyGravity
  , bounce
  , bounceLeft
  , bounceRight
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
    V ( if (abs vx)> 1 then vx*friction else 0)
      ( if (abs vy)> 1 then vy*friction else 0)
  where
    friction = 0.9

-- dt: seconds , mass, currentVelocity -> newVelocity
applyGravity :: Double -> Velocity -> Velocity
applyGravity dt (V vx vy) = V (vx + dvx) (vy + dvy)
  where
    (dvx, dvy) = (fst gravity * dt, snd gravity * dt)


-- Bounce to 0
-- To use this function transform incoming (Velocity, Point) to
-- the wall reference frame with the Zero on the wall,
-- the X axis along the wall
-- and the Y axis perpendicular and coming out of the wall
bounce :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounce r ((V vx vy), (P px py)) =
    if vy<0 && pby < 0 then
      ( friction (V vx (-vy))  -- New velocity
      , P px (py - 2 * pby)  -- New position
      )
    else
      ( (V vx vy), (P px py))
  where
    pby = py - r

-- Bottom floor bouncing
-- floor Double : the Y coordinate of the floor
-- The radius of the circle bounding
bounceFloor :: Double -> Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounceFloor sceneFloor r ((V vx vy), (P px py)) =
    if vy<0 && pby < sceneFloor then
      let dy = abs $ pby - sceneFloor
      in ( friction (V vx (-vy)), (P px (py + 2 * dy)))  -- Specular bounce
    else
      ( (V vx vy), (P px py))
  where
    pby = py - r


bounceRight :: Double -> Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounceRight sceneRight r ((V vx vy), (P px py)) =
    if vx>0 && pbx > sceneRight then
      let dx = abs $ pbx - sceneRight
      in ( friction (V (-vx) vy), (P (px - 2 * dx) py))  -- Specular bounce
    else
      ( (V vx vy), (P px py))
  where
    pbx = px + r

bounceLeft :: Double -> Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounceLeft sceneLeft r ((V vx vy), (P px py)) =
    if vx<0 && pbx < sceneLeft then
      let dx = abs $ pbx - sceneLeft
      in ( friction (V (-vx) vy), (P (px + 2 * dx) py))  -- Specular bounce
    else
      ( (V vx vy), (P px py))
  where
    pbx = px - r
