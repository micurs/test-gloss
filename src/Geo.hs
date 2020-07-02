module Geo
  ( Vector2D(V)
  , Point2D(P)
  , Coordinate(..)
  , UnitVector2D
  , Frame(..)
  , Dottable(..)
  , pointPlusVector
  , vectorPlusVector
  , unitVector
  , geoMapCouple
  , invert
  ) where

import Graphics.Gloss
import Numeric.Matrix

data Vector2D = V Double Double deriving Show
data UnitVector2D = UV Double Double deriving Show
data Point2D = P Double Double deriving Show

data Frame = FR Point2D UnitVector2D UnitVector2D

zeroPoint :: Point2D
zeroPoint = P 0.0 0.0

unitVector :: Vector2D -> UnitVector2D
unitVector (V i j) = UV (i/l) (j/l)
  where
    l = sqrt $ i*i + j*j

homPoint :: [Double] -> Point2D
homPoint [x,y,k] = P (x/k) (y/k)

homVector :: [Double] -> Vector2D
homVector [x,y,_] = V x y

homUnitVector:: [Double] -> UnitVector2D
homUnitVector [x,y,_] = UV x y

-- Mixed Operations

pointPlusVector :: Point2D -> Vector2D -> Point2D
pointPlusVector (P x y) (V i j) = P (x+i) (y+j)

vectorPlusVector :: Vector2D -> Vector2D -> Vector2D
vectorPlusVector (V i1 j1) (V i2 j2) = V (i1+i2) (j1+j2)

-- Dot Product entities : Vector2D and UnitVector2D

class Dottable a where
  dot :: a -> a -> Double
  scalarDot :: Double -> a -> Vector2D
  angle :: a -> a -> Double

instance Dottable Vector2D where
  dot (V i1 j1) (V i2 j2) = (i1*i2)+(j1+j2)
  scalarDot s (V i j) = V (s*i) (s*j)
  angle v1 v2 = acos $ dot uv1 uv2
    where
      uv1 = unitVector v1
      uv2 = unitVector v1

instance Dottable UnitVector2D where
  dot (UV i1 j1) (UV i2 j2) = (i1*i2)+(j1+j2)
  scalarDot s (UV i j) = V (s*i) (s*j)
  angle uv1 uv2 = acos $ dot uv1 uv2


-- Coordinate (objects with x and y coordinates) : Vector2D, Point2D, UnitVector2D

class Coordinate a where
  toPoint :: a -> Point
  x :: a -> Double
  y :: a -> Double
  geoMap :: Frame -> a -> a

instance Coordinate Point2D where
  toPoint (P x y) = (realToFrac x, realToFrac y)
  x (P x _) = x
  y (P _ y) = y
  geoMap (FR (P ox oy) (UV ix iy) (UV jx jy)) (P x y) =
    P (ix*x + jx*y + ox) (iy*x + jx*y + oy)


instance Coordinate Vector2D where
  toPoint (V x y) = (realToFrac x, realToFrac y)
  x (V x _) = x
  y (V _ y) = y
  geoMap (FR (P ox oy) (UV ix iy) (UV jx jy)) (V x y) =
    V (ix*x + jx*y + ox) (iy*x + jx*y + oy)

instance Coordinate UnitVector2D where
  toPoint (UV x y) = (realToFrac x, realToFrac y)
  x (UV x _) = x
  y (UV _ y) = y
  geoMap (FR (P ox oy) (UV ix iy) (UV jx jy)) (UV x y) =
    unitVector (V (ix*x + jx*y + ox) (iy*x + jx*y + oy))


geoMapCouple :: Coordinate a => Coordinate b => Frame -> (a,b) -> (a,b)
geoMapCouple frame (a,b) = ((geoMap frame a), (geoMap frame b))

invert :: Frame -> Maybe Frame
invert (FR (P ox oy) (UV ix iy) (UV jx jy)) =
    case mm of
      Just m -> Just (FR (homPoint $ col 2 m)
                         (homUnitVector $ col 0 m)
                         (homUnitVector $ col 1 m))
      Nothing -> Nothing
  where
    mm = inv $ fromList
        [ [ ix,  jx,  ox]
        , [ iy,  jy,  oy]
        , [0.0, 0.0, 1.0]
        ]