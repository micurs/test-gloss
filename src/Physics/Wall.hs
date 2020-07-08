module Physics.Wall
  ( Wall(..)
  , wall
  , doubleWall
  ) where

import Geo

data Wall = Wall
  { toWall :: Frame
  , rotAngle :: Double
  , centerPos :: Point2D
  , wallDim :: Double
  , wallFriction :: Double
  , disp :: Point2D
  } deriving Show

--
wall :: Double -> Double -> Double -> Point2D -> Point2D -> Wall
wall friction size angle (P trx try) (P dx dy) =
    Wall { toWall = invert $ trn << rot << dsp << baseFrame
         , rotAngle = angle
         , centerPos = P trx try
         , wallDim = size
         , wallFriction = 1.0 - friction
         , disp = P dx dy
    }
  where
    rot = rotation angle
    trn = translation trx try
    dsp = translation dx dy

doubleWall :: Double -> Double -> Point2D -> [Wall]
doubleWall d a (P px py) =
    [ wall 0.2 40 (a - pi/2) (P px py) (P 5 ((d/2)-4) )
    , wall 0.2 40 (a + pi/2) (P px py) (P (-5) ((d/2)-4) )
    , wall 0.2 d   a      (P px py) (P 0 10)
    , wall 0.2 d (a - pi) (P px py) (P 0 20)
    ]
