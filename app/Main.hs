module Main where

import System.Random
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Physics
import Geo
-- GAME state definition

data GameState = Game
  { randomVels :: ([Double],[Double])
  , particles :: ParticleSys
  , walls :: [Wall]
  , counter :: Int
  } deriving Show

getParticles :: GameState -> [Particle]
getParticles (Game _ (ParticleSys particles) _ _) = particles

getWalls :: GameState -> [Wall]
getWalls (Game _ _ walls _) = walls

addParticle :: GameState -> Particle -> GameState
addParticle (Game rv ps w c) p
  = Game { randomVels = rv
         , particles = addParticle2Sys ps p
         , walls = w
         , counter = c}

-- Define the initial state of the game

sceneFloor :: Double
sceneFloor = -480

sceneLeft :: Double
sceneLeft = -480

sceneRight :: Double
sceneRight = 480

windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 1000) (0, 0)

-- Walls to be added into our scene

leftWall = wall 0.1 1000 (-(pi/2)) (P (-480) 0)
rightWall = wall 0.1 1000 (pi/2) (P 480 0)
floorWall = wall 0.2 1000 0 (P 0 (-460))
ceilWall = wall 0.05 1000 pi (P 0 490)

(ramp1WallUp, ramp1WallDown) = doubleWall 500    (pi/22) (P   200  (-200))
(ramp2WallUp, ramp2WallDown) = doubleWall 500 ((-pi)/22) (P (-200) (-100))
(ramp3WallUp, ramp3WallDown) = doubleWall 260    (pi/5) (P   250  (180))
(ramp4WallUp, ramp4WallDown) = doubleWall 260 ((-pi)/5) (P (-250) (180))
(sep1WallUp, sep1WallDown) = doubleWall 80 (pi/2-pi/10) (P 25 (-430))
(sep2WallUp, sep2WallDown) = doubleWall 80 (pi/2+pi/10) (P (-25) (-430))

(obsWallUp, obsWallDown) = doubleWall 100 0.0 (P 0 (30))

-- The initial state

initialState :: StdGen -> GameState
initialState g = Game
  { randomVels = ((randomRs ((-200.0), 200.0) g), (randomRs (100.0, 500.0) g))
  , particles = ParticleSys
    [ ]
  , walls =
    [ leftWall
    , rightWall
    , ceilWall
    , ramp1WallUp
    , ramp1WallDown
    , ramp2WallUp
    , ramp2WallDown
    , ramp3WallUp
    , ramp3WallDown
    , ramp4WallUp
    , ramp4WallDown
    , sep1WallUp
    , sep1WallDown
    , sep2WallUp
    , sep2WallDown
    , obsWallUp
    , obsWallDown
    , floorWall
    -- , wall1
    -- , wall2
    -- , wall3
    -- , wall4
    -- , Wall (frame (P 0.0 480) (unitVector (V (-1.0) 0.0)) (unitVector (V 0.0 (-1.0))))
    ]
  , counter = 0
  }


-- Computes the bounce against a Wall of a circle of a given radius and velocity and position
bounceWall :: Wall -> Double -> (Velocity, Point2D, Point2D) -> (Velocity, Point2D, Point2D)
bounceWall w r vpp =
    geoMapTriple fromWall bouncedP
  where
    fromWall = wallFrame w
    size = wallDim w
    toWall = invert $ fromWall
    wallP = geoMapTriple toWall vpp
    bouncedP = bounce w r wallP

-- RENDERING functions

renderParticle :: Particle -> Picture
renderParticle p =
  Translate x y
    $ Pictures
      [ Color bcolor $ thickCircle 1 $ double2Float r
      , Color white $ thickCircle 1 $ double2Float (r - 5.0)
      ]
  where
    (x, y) = toPoint $ position p
    r = mass p
    bcolor = if life p > 200 then blue else red

renderWall :: Wall -> Picture
renderWall w =
    Color black $ transform $ rectangleSolid ww 5
  where
    o = centerPos w
    ox = double2Float $ x o
    oy = double2Float $ y o
    ww = double2Float $ wallDim w
    angle = double2Float $ (-1) * (rotAngle w) * (180/pi)
    transform = Translate ox oy . Rotate angle

render :: GameState -> Picture
render game =
  Pictures
    [ Pictures (map renderParticle particles)
    , Pictures (map renderWall walls)
    ]
  where
    particles = getParticles game
    walls = getWalls game
    (P x y) = position $ head $ getParticles game
    vel = velocity $ head $ getParticles game

-- INPUT

input :: Event -> GameState -> GameState
-- input (EventKey (SpecialKey KeyRight) Down _ _) game = game { ballVelocity = ( bvX+20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyLeft) Down _ _) game = game { ballVelocity = ( bvX-20, bvY )}
--   where (bvX, bvY) = ballVelocity game
input (EventKey (SpecialKey KeyUp) Down _ _) game =
    addParticle newGame
      $ Particle { position = P 0 200
                      , velocity = V vx vy
                      , mass = 20
                      , life = 2000
                      }
  where
    c = (counter game) + 1
    ((vx: velXs), (vy:velYs)) = randomVels game
    newGame = game
      { randomVels = (velXs, velYs)
      , counter = c
      }

--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyDown) Down _ _) game = game { ballVelocity = ( bvX, bvY-20 )}
--   where (bvX, bvY) = ballVelocity game
input _ game = game

-- SIMULATION step

updateParticle :: Float -> [Wall] -> Particle -> Particle
updateParticle tm (firstWall : otherWalls) particle = particle
  { position = fPos
  , velocity = newVel
  , life = (life particle) - 1
  }
  where
    r = mass particle / 2
    pos = position particle
    vel = velocity particle
    time = float2Double tm
    newVelPos = (vel, pos, movePos time pos vel)
    (fVel, _, fPos) = foldl
        (\vp wall -> bounceWall wall r vp)
        (bounceWall firstWall r newVelPos)
        otherWalls
    newVel = applyGravity (float2Double tm) fVel

movePos :: Double -> Point2D -> Velocity -> Point2D
movePos tm (P px py) (V vx vy) =
  P (px + vx * tm)  (py + vy * tm)

update :: Float -> GameState -> GameState
update tm game
  | length pl == 0 = game
  | otherwise = game {
      particles = ParticleSys
        $ filter (\p -> life p > 0)
        $ map (updateParticle tm walls) pl
    }
  where
    walls = getWalls game
    pl = getParticles game

-- MAIN game setup

fps :: Int
fps = 60

main :: IO ()
main = do
  g <- getStdGen
  play
    windowDisplay
    (dark blue)
    fps
    (initialState g)
    render
    input
    update


