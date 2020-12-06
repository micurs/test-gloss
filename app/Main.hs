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
  , obstructions :: [Obstruction]
  , counter :: Int
  } deriving Show

getParticles :: GameState -> [Particle]
getParticles (Game _ (ParticleSys particles) _ _) = particles

getWalls :: GameState -> [Wall]
getWalls (Game _ _ obstructions _) =
  map (\(W w) -> w) $ filter isWall obstructions

getCircles :: GameState -> [Circle]
getCircles (Game _ _ obstructions _) =
  map (\(C c) -> c) $ filter isCircle obstructions

addParticle :: GameState -> Particle -> GameState
addParticle game p
  = game { particles = addParticle2Sys ps p }
  where
    ps = particles game

-- Define the initial state of the game

sceneFloor :: Double
sceneFloor = -480

sceneLeft :: Double
sceneLeft = -480

sceneRight :: Double
sceneRight = 480

windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 1500) (0, 0)

-- Walls to be added into our scene

doubleWall :: Double -> Double -> Double -> Point2D -> [Obstruction]
doubleWall f d a (P px py) =
    [ W wTop, W wBottom, C cr, C cl ]
  where
    -- w1 = wall 0.1 40 (a - pi/2) (P px py) (P 5 ((d/2)-4) )
    -- w2 = wall 0.1 40 (a + pi/2) (P px py) (P (-5) ((d/2)-4) )
    trn = geoMap $ (translation px py) << (rotation a)
    cr = createCircle 15 $ trn (P (d/2) (-5))
    cl = createCircle 15 $ trn (P (-(d)/2) (-5))
    wTop = wall f d   a      (P px py) (P 0 10)
    wBottom = wall f d (a - pi) (P px py) (P 0 20)

deg2Rad :: Double -> Double
deg2Rad x = pi * x / 180

leftWall = wall 0.05 2000 (-(pi/2)) (P (-480) 0) (P 0 0)
rightWall = wall 0.05 2000 (pi/2) (P 480 0) (P 0 0)
floorWall = wall 0.5 2000 0 (P 0 (-460)) (P 0 0)
ceilWall = wall 0.05 1200 pi (P 0 700) (P 0 0)

ramp1 = doubleWall (-0.2) 250 (deg2Rad 30) (P 320 150)
ramp2 = doubleWall (-0.2) 250 (deg2Rad (-30)) (P (-320) 150)

ramp3 = doubleWall (-0.05) 230 (deg2Rad 10) (P 170 (-100))
ramp4 = doubleWall (-0.05) 230 (deg2Rad (-10)) (P (-170) (-100))

ramp5 = doubleWall 0.4 200 (deg2Rad (70)) (P 120 (-400))
ramp6 = doubleWall 0.4 200 (deg2Rad (-70)) (P (-120) (-400))


c1 = createCircle 50 (P 0 180)
c2 = createCircle 70 (P (-200) 420)
c3 = createCircle 70 (P 200 420)

-- The initial state

initialState :: StdGen -> GameState
initialState g = Game
  { randomVels = ((randomRs ((-200.0), 200.0) g), (randomRs (150.0, 800.0) g))
  , particles = ParticleSys
    [ ]
  , obstructions =
    [ (C c1), (C c2),(C c3)
    ,(W leftWall), (W rightWall), (W ceilWall), (W floorWall) ]
    ++ ramp1 ++ ramp2 ++ ramp3 ++ ramp4 ++ ramp5 ++ ramp6
  , counter = 0
  }


-- Computes the bounce against a Wall of a circle of a given radius and velocity and position
-- bounceWall2 :: Double -> (Velocity, Point2D, Point2D) -> Wall -> (Velocity, Point2D, Point2D)
-- bounceWall2 r vpp w =
--     geoMapTriple fromWallFrame bouncedP
--   where
--     toWallFrame = toWall w
--     fromWallFrame = invert toWallFrame
--     wallP = geoMapTriple toWallFrame vpp
--     bouncedP = bounce w r wallP

bounceWall :: Particle -> Wall -> Particle
bounceWall p w =
    toGlobal bouncedHp
  where
    toWallFrame = toWall w
    toGlobal = geoMapParticle $ invert toWallFrame
    hp = geoMapParticle toWallFrame p
    bouncedHp = bounce w hp


bounceCircle :: Particle -> Circle -> Particle
bounceCircle p c =
    toGlobalFramePart bouncedHp
  where
    pos = position p
    vP2C = (center c) `minus` pos
    frameOrigin = pos .-> (0.5  .> vP2C)
    frameXAxe = unitVector2D vP2C
    ft = frameTransformation $ frame frameOrigin frameXAxe
    toLocalFramePart = geoMapParticle ft
    toGlobalFramePart = geoMapParticle (invert ft)
    toLocalFrameCirc = geoMapCircle ft
    hp = toLocalFramePart p
    hc = toLocalFrameCirc c
    bouncedHp = alignedP2CBounce (hp,hc)


render :: GameState -> Picture
render game =
  Pictures
    [ Pictures (map renderParticle $ getParticles game)
    , Pictures (map renderWall $ getWalls game)
    , Pictures (map renderCircle $ getCircles game)
    ]

-- INPUT

input :: Event -> GameState -> GameState
-- input (EventKey (SpecialKey KeyRight) Down _ _) game = game { ballVelocity = ( bvX+20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyLeft) Down _ _) game = game { ballVelocity = ( bvX-20, bvY )}
--   where (bvX, bvY) = ballVelocity game
input (EventKey (SpecialKey KeyUp) Down _ _) game
  | l >= 400 = newGame
  | otherwise = addParticle newGame
      $ Particle { position = P 0 400
                 , velocity = V vx vy
                 , acceleration = V 0 0
                 , mass = 50        -- (fromIntegral c)
                 , life = 3000
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
updateParticleAgainstCircles :: Double -> [Circle] -> Particle -> Particle
updateParticleAgainstCircles time (firstCircle:otherCircles) particle
  = foldl
      bounceCircle
      (bounceCircle particle firstCircle)
      otherCircles


updateParticleAgainstWalls :: Double -> [Wall] -> Particle -> Particle
updateParticleAgainstWalls time (firstWall : otherWalls) particle
  = foldl
    bounceWall
    (bounceWall particle firstWall)
    otherWalls

  -- = particle { position = fPos
  --            , velocity = newVel
  --            , life = (life particle) - 1
  --            }
  -- where
    -- r = mass particle / 2
    -- pos = position particle
    -- vel = velocity particle
    -- newVelPos = (vel, pos, movePos time vel pos)
    -- (newVel, _, fPos) = foldl
    --     (bounceWall r)
    --     (bounceWall r newVelPos firstWall)
    --     otherWalls

gravity ::Double -> Particle -> Particle
gravity time p =
    p { acceleration = g `add` acc }
  where
    g = time .> gravityAcceleration
    acc = acceleration p

friction :: Double -> Particle -> Particle
friction time p =
    p { acceleration = f `add` (acceleration p) }
  where
    f = (-time * 0.2) .> velocity p
    acc = acceleration p

resetAcc :: Particle -> Particle
resetAcc p = p { acceleration = V 0 0 }

updateVelocity :: Double -> Particle -> Particle
updateVelocity tm p =
    p { velocity = acc `add` vel}
  where
    acc = acceleration p
    vel = velocity p

updatePos :: Double -> Particle -> Particle
updatePos tm p =
    p { position = pos .-> vel, life = l - 1 }
  where
    pos = position p
    vel = tm .> velocity p
    l = life p

updatePipe :: Double -> [Wall] -> [Circle] -> [Particle] -> [Particle]
updatePipe tm walls circles
    = map (updatePos tm)
    . collideParticles
    . map ( (updateParticleAgainstCircles tm circles)
          . (updateParticleAgainstWalls tm walls)
          . (updateVelocity tm)
          . (gravity tm)
          . (friction tm)
          . resetAcc
    )
    . filter (\p -> life p > 0)

update :: Float -> GameState -> GameState
update tm game
  | length pl == 0 = game
  | otherwise = game {
      particles = ParticleSys $ updatePipe time walls circles pl
    }
  where
    walls = getWalls game
    circles = getCircles game
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

