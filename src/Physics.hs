module Physics
  ( Particle (..)
  , ParticleSys (..)
  , Velocity, applyGravity
  , bounceFloor
  , bounceLeft
  , bounceRight
  ) where

import Graphics.Gloss

type Velocity = (Float, Float)

data Particle = Particle
  { position :: Point
  , velocity :: Velocity
  , mass :: Float
  } deriving Show

data ParticleSys = ParticleSys [Particle] deriving Show

gravity :: (Float, Float)
gravity = (0, -98.8)

friction :: Velocity -> Velocity
friction (vx, vy) =
    ( if (abs vx)> 1 then vx*friction else 0
    , if (abs vy)> 1 then vy*friction else 0
    )
  where
    friction = 0.9

-- dt: seconds , mass, currentVelocity -> newVelocity
applyGravity :: Float -> Velocity -> Velocity
applyGravity dt (vx, vy) = (vx + dvx, vy + dvy)
  where
    (dvx, dvy) = (fst gravity * dt, snd gravity * dt)

-- Bottom floor bouncing
-- floor Float : the Y coordinate of the floor
-- The radius of the circle bounding
bounceFloor :: Float -> Float -> (Velocity, Point) -> (Velocity, Point)
bounceFloor sceneFloor r ((vx, vy), (px, py)) =
    if vy<0 && pby < sceneFloor then
      let dy = abs $ pby - sceneFloor
      in ( friction (vx, -vy), (px, py + 2 * dy))  -- Specular bounce
    else
      ( (vx, vy), (px, py))
  where
    pby = py - r


bounceRight :: Float -> Float -> (Velocity, Point) -> (Velocity, Point)
bounceRight sceneRight r ((vx, vy), (px, py)) =
    if vx>0 && pbx > sceneRight then
      let dx = abs $ pbx - sceneRight
      in ( friction (-vx, vy), (px - 2 * dx, py))  -- Specular bounce
    else
      ( (vx, vy), (px, py))
  where
    pbx = px + r

bounceLeft :: Float -> Float -> (Velocity, Point) -> (Velocity, Point)
bounceLeft sceneLeft r ((vx, vy), (px, py)) =
    if vx<0 && pbx < sceneLeft then
      let dx = abs $ pbx - sceneLeft
      in ( friction (-vx, vy), (px + 2 * dx, py))  -- Specular bounce
    else
      ( (vx, vy), (px, py))
  where
    pbx = px - r 
