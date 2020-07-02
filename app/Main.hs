module Main where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Physics
import Geo


-- windowDisplay :: Display
-- windowDisplay = InWindow "Window" (800, 800) (100, 100)

data GameState = Game
  { particles :: ParticleSys
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

initialState :: GameState
initialState = Game
  { particles = ParticleSys
    [ Particle { position = P 0 0
               , velocity = V 200 10    -- 10 unit per second in X and in Y
               , mass = 150
               }
    , Particle { position = P (-150) 0
               , velocity = V 20 230    -- 10 unit per second in X and in Y
               , mass = 60.0
               }
    , Particle { position = P (-150) 110
               , velocity = V (-220) 230    -- 10 unit per second in X and in Y
               , mass = 40.0
               }
    , Particle { position = P 400 300
               , velocity = V 90 100    -- 10 unit per second in X and in Y
               , mass = 30.0
               }
    ]
  }

sceneBounceLeft :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
sceneBounceLeft = bounceLeft sceneLeft

sceneBounceRight :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
sceneBounceRight = bounceRight sceneRight

sceneBounceFloor :: Double -> (Velocity, Point2D) -> (Velocity, Point2D)
sceneBounceFloor r (wVel, wPos) =
  case wallFrameInv of
      Just i -> (geoMap i bLVel, geoMap i bLPos)
      Nothing -> (wVel, wPos)
  where
    wallFrame = FR (P 0.0 sceneFloor) (unitVector (V 0.0 1.0)) (unitVector (V 1.0 0.0))
    wallFrameInv = invert wallFrame
    lVelPos = ( geoMap wallFrame wVel, geoMap wallFrame wPos)
    (bLVel, bLPos) = bounce r lVelPos


getParticles :: GameState -> [Particle]
getParticles (Game (ParticleSys particles)) = particles

render :: GameState -> Picture
render game = let
    particles = getParticles game
    (P x y) = position $ head $ getParticles game
    vel = velocity $ head $ getParticles game
  in
    Pictures
    [ Pictures (map renderParticle particles)
    , Color black $ Translate 0 (double2Float sceneFloor) $ rectangleSolid 1000 5
    , Color black $ Translate (double2Float sceneLeft) 0 $ rectangleSolid 5 1000
    , Color black $ Translate (double2Float sceneRight) 0 $ rectangleSolid 5 1000
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

updateParticle :: Float -> Particle -> Particle
updateParticle tm particle = particle
  { position = fPos
  , velocity = newVel
  }
  where
    r = mass particle / 2
    pos = position particle
    vel = velocity particle
    newPotPos = movePos (float2Double tm) pos vel
    sceneBounce = sceneBounceLeft r . sceneBounceRight r . sceneBounceFloor r
    (fVel, fPos) = sceneBounce $ (vel, newPotPos)
    newVel = applyGravity (float2Double tm) fVel

movePos :: Double -> Point2D -> Velocity -> Point2D
movePos tm (P px py) (V vx vy) =
  P (px + vx * tm)  (py + vy * tm)

update :: Float -> GameState -> GameState
update tm game = game {
    particles = ParticleSys $ map (updateParticle tm) pl
  }
  where
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


