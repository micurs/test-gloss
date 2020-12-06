module Geo.PointVector where

import qualified Numeric.Matrix as M

import Geo.Base

-- Utilities Operations to build Point2D and Vector2D from Matrix

point2DFromMatrix :: M.Matrix Double -> Point2D
point2DFromMatrix m = (P ((M.at m (1, 1))/(M.at m (3, 1))) ((M.at m (2, 1))/(M.at m (3, 1))) )

vector2DFromMatrix :: M.Matrix Double -> Vector2D
vector2DFromMatrix m = (V (M.at m (1, 1)) (M.at m (2, 1)) )

unitVector2DFromMatrix :: M.Matrix Double -> UnitVector2D
unitVector2DFromMatrix m = unitVector2D (V (M.at m (1, 1)) (M.at m (2, 1)) )

-- WithCoordinates (i.e. objects with x and y coordinates) : Vector2D, Point2D, UnitVector2D

instance WithCoordinates Point2D where
  x (P x _) = x
  y (P _ y) = y
  geoMap t (P x y) = P (tx/tk) (ty/tk)
    where
      (tx,ty,tk) = transform t (x,y,1.0)
  geoMapBack t (P x y) = P (tx/tk) (ty/tk)
    where
      (tx,ty,tk) = transformBack t (x,y,1.0)
  fromList [x,y,k] = P (x/k) (y/k)

instance WithCoordinates Vector2D where
  x (V x _) = x
  y (V _ y) = y
  geoMap t (V x y) = V tx ty
    where
      (tx,ty,_) = transform t (x,y,0)
  geoMapBack t (V x y) = V tx ty
    where
      (tx,ty,_) = transformBack t (x,y,0.0)
  fromList [x,y,_] = V x y


instance WithCoordinates UnitVector2D where
  x (UV x _) = x
  y (UV _ y) = y
  geoMap tr (UV i j) = unitVector2D (V ti tj)
    where
      (ti,tj,_) = transform tr (i,j,0)
  geoMapBack t (UV i j) = unitVector2D (V ti tj)
    where
      (ti,tj,_) = transformBack t (i,j,0.0)
  fromList [x,y,_] = unitVector2D (V x y)


-- Direction (i.e objects that point to a direction) : Vector2D and UnitVector2D

instance Direction Vector2D where
  dot (V i1 j1) (V i2 j2) = (i1*i2)+(j1+j2)
  angle v1 v2 = acos $ dot uv1 uv2
    where
      uv1 = unitVector2D v1
      uv2 = unitVector2D v1


instance Direction UnitVector2D where
  dot (UV i1 j1) (UV i2 j2) = (i1*i2)+(j1+j2)
  angle uv1 uv2 = acos $ dot uv1 uv2

-- GeoVector (i.e objects with a direction and intensity): Vector2D

instance GeoVector Vector2D where
  (.>) s (V x y) = V (s*x) (s*y)
  add (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)
  minus p2 p1 = V (x2-x1) (y2-y1)
    where
      x1 = x p1
      x2 = x p2
      y1 = y p1
      y2 = y p2
  vLength (V i j) = sqrt $ i*i + j*j
  vLengthSq (V i j) = i*i + j*j

-- Position (i.e objects that define a point in the space): Point2D

instance Position Point2D where
  toPoint (P x y) = (realToFrac x, realToFrac y)
  (.->) (P px py) d = P (px+dx) (py+dy)
    where
      dx = x d
      dy = y d

-- Eq implementation for Point2D, Vector2D and UnitVector2D

eqToll = 0.000001

instance Eq Point2D where
  (==) (P x1 y1) (P x2 y2) =
    abs (x1-x2) < eqToll && abs (y1-y2) < eqToll

instance Eq Vector2D where
  (==) (V x1 y1) (V x2 y2) =
    abs (x1-x2) < eqToll && abs (y1-y2) < eqToll

instance Eq UnitVector2D where
  (==) (UV x1 y1) (UV x2 y2) =
    abs (x1-x2) < eqToll && abs (y1-y2) < eqToll
