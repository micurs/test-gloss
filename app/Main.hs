module Main where

import Data.List
import System.Random
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Physics
import Geo
import Render

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

deg2Rad :: Double -> Double
deg2Rad x = pi * x / 180

leftWall = wall 0.05 1000 (-(pi/2)) (P (-480) 0) (P 0 0)
rightWall = wall 0.05 1000 (pi/2) (P 480 0) (P 0 0)
floorWall = wall 0.5 1000 0 (P 0 (-460)) (P 0 0)
ceilWall = wall 0.05 1000 pi (P 0 490) (P 0 0)

ramp1 = doubleWall 200 (deg2Rad 5)    (P 0 0)
-- (ramp2WallUp, ramp2WallDown) = doubleWall 200 (deg2Rad (-45)) (P (-300) (0))
-- (ramp3WallUp, ramp3WallDown) = doubleWall 260 (pi/5)          (P   250  (180))
-- (ramp4WallUp, ramp4WallDown) = doubleWall 200 ((-pi)/5)       (P (-250) (180))

sep1 = doubleWall 200 (deg2Rad 110) (P (-150) (-380))
sep2 = doubleWall 200 (deg2Rad (-110)) (P (150) (-380))

-- (obsWallUp, obsWallDown) = doubleWall 100 0.0 (P 0 (30))

-- The initial state

initialState :: StdGen -> GameState
initialState g = Game
  { randomVels = ((randomRs ((-300.0), 300.0) g), (randomRs (150.0, 700.0) g))
  , particles = ParticleSys
    [ ]
  , walls =
    [ leftWall, rightWall, ceilWall, floorWall ]
    -- ++ sep1 ++ sep2
    ++ ramp1
  , counter = 0
  }


-- Computes the bounce against a Wall of a circle of a given radius and velocity and position
bounceWall :: Double -> (Velocity, Point2D, Point2D) -> Wall -> (Velocity, Point2D, Point2D)
bounceWall r vpp w =
    geoMapTriple fromWallFrame bouncedP
  where
    toWallFrame = toWall w
    fromWallFrame = invert toWallFrame
    size = wallDim w
    wallP = geoMapTriple toWallFrame vpp
    bouncedP = bounce w r wallP


render :: GameState -> Picture
render game =
  Pictures
    [ Pictures (map renderParticle $ getParticles game)
    , Pictures (map renderWall $ getWalls game)
    ]

-- INPUT

input :: Event -> GameState -> GameState
-- input (EventKey (SpecialKey KeyRight) Down _ _) game = game { ballVelocity = ( bvX+20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyLeft) Down _ _) game = game { ballVelocity = ( bvX-20, bvY )}
--   where (bvX, bvY) = ballVelocity game
input (EventKey (SpecialKey KeyUp) Down _ _) game
  | l >= 40 = newGame
  | otherwise = addParticle newGame
      $ Particle { position = P 0 200
                 , velocity = V vx vy
                 , acceleration = V 0 0
                 , mass = 30.0 + (fromIntegral c) * 2       -- (fromIntegral c)
                 , life = 5000
                 }
  where
    l = length $ getParticles game
    c = (counter game) + 1
    ((vx: velXs), (vy:velYs)) = randomVels game -- Get the first of the infinite lists of random values
    newGame = game
      { randomVels = (velXs, velYs) -- Update the infinite list with the queues of the original lists!
      , counter = c
      }

--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyDown) Down _ _) game = game { ballVelocity = ( bvX, bvY-20 )}
--   where (bvX, bvY) = ballVelocity game
input _ game = game

-- SIMULATION step

updateParticle :: Double -> [Wall] -> Particle -> Particle
updateParticle time (firstWall : otherWalls) particle = particle
  { position = fPos
  , velocity = newVel
  , life = (life particle) - 1
  }
  where
    r = mass particle / 2
    pos = position particle
    vel = velocity particle
    newVelPos = (vel, pos, movePos time vel pos)
    (newVel, _, fPos) = foldl
        (bounceWall r)
        (bounceWall r newVelPos firstWall)
        otherWalls

gravityParticle ::Double -> Particle -> Particle
gravityParticle time p =
    p { velocity = applyGravity time (velocity p)}




update :: Float -> GameState -> GameState
update tm game
  | length pl == 0 = game
  | otherwise = game {
      particles = ParticleSys
        $ collideParticles
        $ ( map (updateParticle time walls)
          . map (gravityParticle time)
          . filter (\p -> life p > 0)
        )
        pl
    }
  where
    walls = getWalls game
    pl = getParticles game
    time = float2Double tm

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

