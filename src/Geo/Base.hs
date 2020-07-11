module Geo.Base
  ( Vector2D(V)
  , Point2D(P)
  , UnitVector2D(UV)
  , WithCoordinates(..)
  , Transformer(..)
  , Position(..)
  , Direction(..)
  , GeoVector(..)
  , RefFrame(..)
  , zeroPoint
  , unitVector2D
  , unitVector2DX
  , unitVector2DY
  , unitVector2DNX
  , unitVector2DNY
  ) where

import qualified Numeric.Matrix as M
import Graphics.Gloss

-- ============================================================================

data Vector2D = V Double Double deriving (Show, Eq)
data UnitVector2D = UV Double Double deriving (Show, Eq)
data Point2D = P Double Double deriving (Show, Eq)


-- ========================== Point2D =========================================

zeroPoint :: Point2D
zeroPoint = P 0.0 0.0

-- ========================== Vector2D ========================================


-- ========================== UnitVector2D ====================================

unitVector2DX = UV 1.0 0.0
unitVector2DNX = UV (-1.0) 0.0

unitVector2DY = UV 0.0 1.0
unitVector2DNY = UV 0.0 (-1.0)

unitVector2D :: Vector2D -> UnitVector2D
unitVector2D (V i j) = UV (i/l) (j/l)
  where
    l = sqrt $ i*i + j*j


-- ======================== Classes for positions and directions =========================== ---

class WithCoordinates a where
  x :: a -> Double
  y :: a -> Double
  geoMap :: Transformer t => t -> a -> a
  geoMapBack :: Transformer t => t -> a -> a
  fromList :: [Double] -> a

class WithCoordinates a => Position a where
  toPoint :: a -> Point
  (.->):: GeoVector d => a -> d -> a

class WithCoordinates a => Direction a where
  dot :: a -> a -> Double
  angle :: a -> a -> Double

class Direction d => GeoVector d where
  (.>) :: Double -> d -> d
  add:: d -> d -> d
  minus:: Position p => p -> p -> d
  vLength :: d -> Double
  vLengthSq :: d -> Double


-- ======================== Classes for transformation =========================== ---

class Transformer a where
  directM:: a -> M.Matrix Double
  inverseM :: a -> M.Matrix Double
  invert:: a -> a
  -- compose :: Transformer b => a -> a -> b
  -- (<<) :: Transformer b => a -> a -> b
  transform :: a -> (Double, Double, Double) -> (Double, Double, Double)
  transformBack :: a -> (Double, Double, Double) -> (Double, Double, Double)

class Transformer f => RefFrame f where
  origin :: f -> Point2D
  xAxe :: f -> UnitVector2D
  yAxe :: f -> UnitVector2D
