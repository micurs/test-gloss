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
windowDisplay = InWindow "Window" (1000, 2000) (0, 0)

-- Walls to be added into our scene

leftWall = wall 1000 (-(pi/2)) (P (-480) 0)
rightWall = wall 1000 (pi/2) (P 480 0)
floorWall = wall 1000 0 (P 0 (-480))
ceilWall = wall 1000 pi (P 0 490)

ramp1Wall = wall 400    (pi/9) (P   280  (-300))
ramp2Wall = wall 400 ((-pi)/6) (P (-300) (-120))

-- The initial state

initialState :: StdGen -> GameState
initialState g = Game
  { randomVels = ((randomRs ((-200.0), 200.0) g), (randomRs (100.0, 500.0) g))
  , particles = ParticleSys
    [ Particle { position = P 10 50
               , velocity = V (-100) 150    -- 10 unit per second in X and in Y
               , mass = 22
               }
    , Particle { position = P (-10) 50
               , velocity = V 100 110    -- 10 unit per second in X and in Y
               , mass = 25
               }
    , Particle { position = P (-190) 100
               , velocity = V (-20) 200    -- 10 unit per second in X and in Y
               , mass = 32
               }
    , Particle { position = P 0 0
               , velocity = V 120 430    -- 10 unit per second in X and in Y
               , mass = 15
               }
    -- , Particle { position = P (350) 400
    --            , velocity = V 20 390    -- 10 unit per second in X and in Y
    --            , mass = 20.0
    --            }
    -- , Particle { position = P (-10) 110
    --            , velocity = V (-42) 3130    -- 10 unit per second in X and in Y
    --            , mass = 40.0
    --            }
    -- , Particle { position = P 400 300
    --            , velocity = V 90 10    -- 10 unit per second in X and in Y
    --            , mass = 30.0
    --            }
    ]
  , walls =
    [ floorWall
    , leftWall
    , rightWall
    , ceilWall
    , ramp1Wall
    , ramp2Wall
    -- , wall1
    -- , wall2
    -- , wall3
    -- , wall4
    -- , Wall (frame (P 0.0 480) (unitVector (V (-1.0) 0.0)) (unitVector (V 0.0 (-1.0))))
    ]
  , counter = 0
  }


-- Computes the bounce against a Wall of a circle of a given radius and velocity and position
bounceWall :: Wall -> Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounceWall w r p =
    geoMapCouple fromWall bouncedP
  where
    fromWall = wallFrame w
    toWall = invert $ fromWall
    wallP = geoMapCouple toWall p
    bouncedP = bounce r wallP

-- RENDERING functions

renderParticle :: Particle -> Picture
renderParticle p = Translate x y $ Pictures
          [ Color red $ thickCircle 1 $ double2Float r
          , Color white $ thickCircle 1 $ double2Float (r - 5.0)
          ]
        where
          (x, y) = toPoint $ position p
          r = mass p

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
render game = let
    particles = getParticles game
    walls = getWalls game
    (P x y) = position $ head $ getParticles game
    vel = velocity $ head $ getParticles game
  in
    Pictures
    [ Pictures (map renderParticle particles)
    , Pictures (map renderWall walls)
    , Translate (-480.0) (460) $ Scale 0.15 0.15 $ Color yellow $ text $ "Position[0]:" ++ (show (x, y))
    , Translate (-480.0) (440) $ Scale 0.15 0.15 $ Color yellow $ text $ "Velocity[0]:" ++ (show vel)
    ]

-- INPUT

input :: Event -> GameState -> GameState
-- input (EventKey (SpecialKey KeyRight) Down _ _) game = game { ballVelocity = ( bvX+20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyLeft) Down _ _) game = game { ballVelocity = ( bvX-20, bvY )}
--   where (bvX, bvY) = ballVelocity game
input (EventKey (SpecialKey KeyUp) Down _ _) game =
    addParticle newGame
      $ Particle { position = P 0 0
                      , velocity = V vx vy
                      , mass = 20
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
  }
  where
    r = mass particle / 2
    pos = position particle
    vel = velocity particle
    time = float2Double tm
    newVelPos = (vel, movePos time pos vel)
    (fVel, fPos) = foldl
        (\vp wall -> bounceWall wall r vp)
        (bounceWall firstWall r newVelPos)
        otherWalls
    newVel = applyGravity (float2Double tm) fVel

movePos :: Double -> Point2D -> Velocity -> Point2D
movePos tm (P px py) (V vx vy) =
  P (px + vx * tm)  (py + vy * tm)

update :: Float -> GameState -> GameState
update tm game = game {
    particles = ParticleSys $ map (updateParticle tm walls) pl
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


