module Geo
  ( Vector2D(V)
  , Point2D(P)
  , Coordinate(..)
  , UnitVector2D
  , Dottable(..)
  , pointPlusVector
  , vectorPlusVector
  , unitVector
  ) where

import Graphics.Gloss

data Vector2D = V Float Float
data UnitVector2D = UV Float Float
data Point2D = P Float Float

data HalfPlane2D = HP
  { normal :: UnitVector2D
  , position :: Point2D
  }

zeroPoint :: Point2D
zeroPoint = P 0.0 0.0

unitVector :: Vector2D -> UnitVector2D
unitVector (V i j) = UV (i/l) (j/l)
  where
    l = sqrt $ i*i + j*j

-- Mixed Operations

pointPlusVector :: Point2D -> Vector2D -> Point2D
pointPlusVector (P x y) (V i j) = P (x+i) (y+j)

vectorPlusVector :: Vector2D -> Vector2D -> Vector2D
vectorPlusVector (V i1 j1) (V i2 j2) = V (i1+i2) (j1+j2)

-- Dot Product entities : Vector2D and UnitVector2D

class Dottable a where
  dot :: a -> a -> Float
  scalarDot :: Float -> a -> Vector2D
  angle :: a -> a -> Float

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
  x :: a -> Float
  y :: a -> Float

instance Coordinate Point2D where
  toPoint (P x y) = (x,y)
  x (P x _) = x
  y (P _ y) = y

instance Coordinate Vector2D where
  toPoint (V x y) = (x,y)
  x (V x _) = x
  y (V _ y) = y

instance Coordinate UnitVector2D where
  toPoint (UV x y) = (x,y)
  x (UV x _) = x
  y (UV _ y) = y


-- Build a half plane (empty, full) using a distance (from 0)
-- and a vector indicating the perpendicular to the separation line
-- the vector point in the direction of empty
halfPlane2D :: Float -> UnitVector2D -> HalfPlane2D
halfPlane2D w (UV i j) = HP
  { normal = UV i j
  , position = pointPlusVector zeroPoint $ scalarDot w (UV i j)
  }


