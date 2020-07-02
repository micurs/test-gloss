module Main where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Physics
import Geo


-- windowDisplay :: Display
-- windowDisplay = InWindow "Window" (800, 800) (100, 100)

data Wall = Wall Frame deriving Show

data GameState = Game
  { particles :: ParticleSys
  , walls :: [Wall]
  } deriving Show

sceneFloor :: Double
sceneFloor = -480

sceneLeft :: Double
sceneLeft = -480

sceneRight :: Double
sceneRight = 480

windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 2000) (0, 0)

renderParticle :: Particle -> Picture
renderParticle p = Translate x y $ Pictures
          [ Color red $ thickCircle 1 $ double2Float r
          , Color white $ thickCircle 1 $ double2Float (r - 5.0)
          ]
        where
          (x, y) = toPoint $ position p
          r = mass p

renderWall :: Wall -> Picture
renderWall (Wall (FR o i j)) =
  Color black $ transform $ rectangleSolid 10000 5
  where
    ox = double2Float $ x o
    oy = double2Float $ y o
    angle = 90.0 - (acos (x j) * (180.0/pi))
    transform = Translate ox oy . Rotate (double2Float angle)
-- , Color black $ Translate 0 (double2Float sceneFloor) $ rectangleSolid 1000 5

angle1 = - (pi / 6)
angle2 = (pi / 3)

initialState :: GameState
initialState = Game
  { particles = ParticleSys
    [ Particle { position = P 0 400
               , velocity = V 580 0    -- 10 unit per second in X and in Y
               , mass = 50
               }
    , Particle { position = P (-150) 100
               , velocity = V 320 4230    -- 10 unit per second in X and in Y
               , mass = 60.0
               }
    , Particle { position = P (-190) 100
               , velocity = V 20 500    -- 10 unit per second in X and in Y
               , mass = 40.0
               }
    , Particle { position = P (100) 400
               , velocity = V 120 3230    -- 10 unit per second in X and in Y
               , mass = 10.0
               }
    , Particle { position = P (350) 400
               , velocity = V 20 390    -- 10 unit per second in X and in Y
               , mass = 20.0
               }
    , Particle { position = P (-10) 110
               , velocity = V (-42) 3130    -- 10 unit per second in X and in Y
               , mass = 40.0
               }
    , Particle { position = P 400 300
               , velocity = V 90 10    -- 10 unit per second in X and in Y
               , mass = 30.0
               }
    ]
  , walls =
    [ Wall (FR (P 0.0 (-480)) (unitVector (V 1.0 0.0)) (unitVector (V 0.0 1.0)))
    , Wall (FR (P 480.0 0.0) (unitVector (V 0.0 1.0)) (unitVector (V (-1.0) 0.0)))
    , Wall (FR (P (-480) 0.0) (unitVector (V 0.0 (-1.0))) (unitVector (V 1.0 0.0)))
    , Wall (FR (P (200) (-300)) (unitVector (V (cos (angle2)) (sin (angle2)))) (unitVector (V (-(sin (angle2))) (cos (angle2)))))
    , Wall (FR (P (-200) (-400)) (unitVector (V (cos (angle1)) (sin (angle1)))) (unitVector (V (-(sin (angle1))) (cos (angle1)))))
    , Wall (FR (P 0.0 480) (unitVector (V (-1.0) 0.0)) (unitVector (V 0.0 (-1.0))))
    ]
  }

sceneBounceLeft :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
sceneBounceLeft r (wVel, wPos) =
    geoMapCouple fromLeftFrame bouncedCircle
  where
    fromLeftFrame = FR (P sceneLeft 0.0) (unitVector (V 0.0 (-1.0))) (unitVector (V 1.0 0.0))
    toLeftFrame = invert fromLeftFrame
    lVelPos = case toLeftFrame of
      Just i -> (geoMap i wVel, geoMap i wPos)
      Nothing -> (wVel, wPos)
    bouncedCircle = bounce r lVelPos

sceneBounceRight :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
-- sceneBounceRight = bounceRight sceneRight
sceneBounceRight r (wVel, wPos) =
    geoMapCouple fromRightFrame bouncedCircle
  where
    fromRightFrame = FR (P sceneRight 0.0) (unitVector (V 0.0 1.0)) (unitVector (V (-1.0) 0.0))
    toRightFrame = invert fromRightFrame
    lVelPos = case toRightFrame of
      Just i -> (geoMap i wVel, geoMap i wPos)
      Nothing -> (wVel, wPos)
    bouncedCircle = bounce r lVelPos


sceneBounceFloor :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
sceneBounceFloor r (wVel, wPos) =
    geoMapCouple fromFloorFrame bouncedCircle
  where
    fromFloorFrame = FR (P 0.0 sceneFloor) (unitVector (V 1.0 0.0)) (unitVector (V 0.0 1.0))
    toFloorFrame = invert fromFloorFrame
    lVelPos = case toFloorFrame of
      Just i -> ( geoMap i wVel, geoMap i wPos)
      Nothing -> (wVel, wPos)
    bouncedCircle = bounce r lVelPos

bounceWall :: Wall -> Double -> (Velocity, Point2D) -> (Velocity, Point2D)
bounceWall (Wall wallFrame) r p =
    geoMapCouple wallFrame bouncedP
  where
    toWall = invert wallFrame
    wallP = case toWall of
      Just i -> geoMapCouple i p
      Nothing -> p
    bouncedP = bounce r wallP

getParticles :: GameState -> [Particle]
getParticles (Game (ParticleSys particles) _) = particles

getWalls :: GameState -> [Wall]
getWalls (Game _ walls) = walls

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
    -- , Color black $ Translate 0 (double2Float sceneFloor) $ rectangleSolid 1000 5
    -- , Color black $ Translate (double2Float sceneLeft) 0 $ rectangleSolid 5 1000
    -- , Color black $ Translate (double2Float sceneRight) 0 $ rectangleSolid 5 1000
    , Translate (-480.0) (460) $ Scale 0.15 0.15 $ Color yellow $ text $ "Position[0]:" ++ (show (x, y))
    , Translate (-480.0) (440) $ Scale 0.15 0.15 $ Color yellow $ text $ "Velocity[0]:" ++ (show vel)
    ]

input :: Event -> GameState -> GameState
-- input (EventKey (SpecialKey KeyRight) Down _ _) game = game { ballVelocity = ( bvX+20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyLeft) Down _ _) game = game { ballVelocity = ( bvX-20, bvY )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyUp) Down _ _) game = game { ballVelocity = ( bvX, bvY+20 )}
--   where (bvX, bvY) = ballVelocity game
-- input (EventKey (SpecialKey KeyDown) Down _ _) game = game { ballVelocity = ( bvX, bvY-20 )}
--   where (bvX, bvY) = ballVelocity game
input _ game = game

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
    -- sceneBounce = sceneBounceLeft r . sceneBounceRight r . sceneBounceFloor r
    -- (fVel, fPos) = sceneBounce $ (vel, newPotPos)
    newVel = applyGravity (float2Double tm) fVel
    -- newVel = fVel

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

fps :: Int
fps = 60

main :: IO ()
main = do
  print "Gravity rules!"
  -- display windowDisplay (dark blue) scene
  play
    windowDisplay
    (dark blue)
    fps
    initialState
    render
    input
    update


