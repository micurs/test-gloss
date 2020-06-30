module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Physics


-- windowDisplay :: Display
-- windowDisplay = InWindow "Window" (800, 800) (100, 100)

data GameState = Game
  { particles :: ParticleSys
  } deriving Show

sceneFloor :: Float
sceneFloor = -480

sceneLeft :: Float
sceneLeft = -480

sceneRight :: Float
sceneRight = 480

windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 2000) (0, 0)

renderParticle :: Particle -> Picture
renderParticle p = Translate x y $ Pictures
          [ Color red $ thickCircle 1 r
          , Color white $ thickCircle 1 (r-5)
          ]
        where
          (x, y) = position p
          r = mass p

initialState :: GameState
initialState = Game
  { particles = ParticleSys
    [ Particle { position = (0,0)
               , velocity = (200,10)    -- 10 unit per second in X and in Y
               , mass = 150
               }
    , Particle { position = (-150,0)
               , velocity = (20,230)    -- 10 unit per second in X and in Y
               , mass = 60.0
               }
    , Particle { position = (-150,110)
               , velocity = (-220,230)    -- 10 unit per second in X and in Y
               , mass = 40.0
               }
    , Particle { position = (400,300)
               , velocity = (90,100)    -- 10 unit per second in X and in Y
               , mass = 30.0
               }
    ]
  }

sceneBounceLeft :: Float -> (Velocity, Point) -> (Velocity, Point)
sceneBounceLeft = bounceLeft sceneLeft

sceneBounceRight :: Float -> (Velocity, Point) -> (Velocity, Point)
sceneBounceRight = bounceRight sceneRight

sceneBounceFloor :: Float -> (Velocity, Point) -> (Velocity, Point)
sceneBounceFloor = bounceFloor sceneFloor


getParticles :: GameState -> [Particle]
getParticles (Game (ParticleSys particles)) = particles

render :: GameState -> Picture
render game = let
    particles = getParticles game
    (x,y) = position $ head $ getParticles game
    vel = velocity $ head $ getParticles game
  in
    Pictures
    [ Pictures (map renderParticle particles)
    , Color black $ Translate 0 sceneFloor $ rectangleSolid 1000 5
    , Color black $ Translate sceneLeft 0 $ rectangleSolid 5 1000
    , Color black $ Translate sceneRight 0 $ rectangleSolid 5 1000
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
    newPotPos = movePos tm pos vel
    sceneBounce = sceneBounceLeft r . sceneBounceRight r . sceneBounceFloor r
    (fVel, fPos) = sceneBounce $ (vel, newPotPos)
    newVel = applyGravity tm fVel

movePos :: Float -> Point -> Velocity -> Point
movePos tm (px, py) (vx, vy) =
  (px + vx * tm, py + vy * tm)

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


