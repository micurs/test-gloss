module Render
  ( renderParticle
  , renderWall
  , renderCircle
  ) where

import GHC.Float
import Graphics.Gloss

import Physics
import Geo

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
    bcolor = if life p > 200 then black else red

renderCircle :: Circle -> Picture
renderCircle c =
  Translate x y
    $ Pictures
      [ Color yellow $ thickCircle 1 r
      , Color (greyN 0.2) $ thickCircle 1 (r - 5.0)
      ]
  where
    (x, y) = toPoint $ center c
    r = double2Float $ 2 * (radius c)


renderWall :: Wall -> Picture
renderWall w =
    Pictures
      [ transform $ Pictures
        [ Color (greyN 0.2) $ Translate 0 (-5) $ rectangleSolid ww 10
        , Color yellow $ transformTop $ Translate 0 (-5) $ rectangleSolid ww 1
        ]
      ]
  where
    o = centerPos w
    d = disp w
    ox = double2Float $ x o
    oy = double2Float $ y o
    dispX = double2Float $ x d
    dispY = double2Float $ y d
    ww = double2Float $ wallDim w
    angle = double2Float $ (-1) * (rotAngle w) * (180/pi)
    transform = Translate ox oy . Rotate angle . Translate dispX dispY
    transformTop = Translate 0 5

