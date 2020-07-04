module Physics.Wall
  ( Wall(..)
  , wall
  ) where

import Geo

data Wall = Wall
  { wallFrame :: Frame
  , rotAngle :: Double
  , centerPos :: Point2D
  , wallDim :: Double
  } deriving Show

wall :: Double -> Double -> Point2D -> Wall
wall d angle (P trx try) =
    Wall { wallFrame = compose trn $ compose rot baseFrame
         , rotAngle = angle
         , centerPos = (P trx try)
         , wallDim = d
    }
  where
    rot = rotation angle
    trn = translation trx try
    baseFrame = frame (P 0 0) unitVectorX unitVectorY
