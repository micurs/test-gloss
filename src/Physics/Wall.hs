module Physics.Wall
  ( Wall(..)
  , wall
  ) where

import Geo

data Wall = Wall
  { toWall :: Transformation
  , rotAngle :: Double
  , centerPos :: Point2D
  , wallDim :: Double
  , wallFriction :: Double
  , disp :: Point2D
  } deriving Show

--
wall :: Double -> Double -> Double -> Point2D -> Point2D -> Wall
wall friction size angle (P trx try) (P dx dy) = Wall
    { toWall = invert $ trn << rot << dsp << baseFrame
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

