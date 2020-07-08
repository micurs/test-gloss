module Geo.Basic
  ( Vector2D(V)
  , Point2D(P)
  , UnitVector2D(UV)
  , Transformation(..)
  , Frame
  , zeroPoint
  , unitVector
  , unitVectorX
  , unitVectorY
  , unitVectorNX
  , unitVectorNY
  , vectorLength
  , vectorLengthSq
  ) where

import Numeric.Matrix

-- ============================================================================

data Vector2D = V Double Double deriving (Show, Eq)
data UnitVector2D = UV Double Double deriving (Show, Eq)
data Point2D = P Double Double deriving (Show, Eq)

data Transformation = TR
  { direct :: Matrix Double
  , inverse :: Matrix Double
  } deriving Show

type Frame = Transformation

-- ========================== Point2D =========================================

zeroPoint :: Point2D
zeroPoint = P 0.0 0.0


-- ========================== Vector2D ========================================

vectorLength :: Vector2D -> Double
vectorLength (V i j) = sqrt $ i*i + j*j

vectorLengthSq :: Vector2D -> Double
vectorLengthSq (V i j) = i*i + j*j

-- ========================== UnitVector2D ====================================

unitVectorX = UV 1.0 0.0
unitVectorNX = UV (-1.0) 0.0

unitVectorY = UV 0.0 1.0
unitVectorNY = UV 0.0 (-1.0)

unitVector :: Vector2D -> UnitVector2D
unitVector (V i j) = UV (i/l) (j/l)
  where
    l = sqrt $ i*i + j*j
