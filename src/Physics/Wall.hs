module Physics.Wall
  ( Wall(..)
  , wall
  , doubleWall
  ) where

import Geo

data Wall = Wall
  { wallFrame :: Frame
  , rotAngle :: Double
  , centerPos :: Point2D
  , wallDim :: Double
  , wallFriction :: Double
  } deriving Show

wall :: Double -> Double -> Double -> Point2D -> Wall
wall f d angle (P trx try) =
    Wall { wallFrame = compose trn $ compose rot baseFrame
         , rotAngle = angle
         , centerPos = (P trx try)
         , wallDim = d
         , wallFriction = 1.0 - f
    }
  where
    rot = rotation angle
    trn = translation trx try
    baseFrame = frame (P 0 0) unitVectorX unitVectorY

doubleWall :: Double -> Double -> Point2D -> ( Wall, Wall )
doubleWall d a (P px py) =
    ( wall 0.4 d a (P px py)
    , wall 0.1 d (a + pi) (P px (py-4))
    )
