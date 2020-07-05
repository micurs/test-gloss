{-# LANGUAGE TypeSynonymInstances #-}

module Geo
  ( Vector2D(V)
  , Point2D(P)
  , Transformation(TR)
  , Coordinate(..)
  , UnitVector2D
  , Dottable(..)
  , Frame
  , GeoMapper(..)
  , pointPlusVector
  , vectorPlusVector
  , unitVector
  , unitVectorX
  , unitVectorY
  , unitVectorNX
  , unitVectorNY
  , frame
  , geoMapCouple
  , geoMapTriple
  , origin
  , iaxe
  , jaxe
  , rotation
  , translation
  ) where

import Graphics.Gloss
import Numeric.Matrix

-- ============================================================================

data Vector2D = V Double Double deriving Show
data UnitVector2D = UV Double Double deriving Show
data Point2D = P Double Double deriving Show

data Transformation = TR
  { direct :: Matrix Double
  , inverse :: Matrix Double
  } deriving Show

-- ============================================================================

zeroPoint :: Point2D
zeroPoint = P 0.0 0.0

unitVector :: Vector2D -> UnitVector2D
unitVector (V i j) = UV (i/l) (j/l)
  where
    l = sqrt $ i*i + j*j

unitVectorX = UV 1.0 0.0
unitVectorNX = UV (-1.0) 0.0

unitVectorY = UV 0.0 1.0
unitVectorNY = UV 0.0 (-1.0)

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

point2DFromMatrix :: Matrix Double -> Point2D
point2DFromMatrix m = (P ((at m (1, 1))/(at m (3, 1))) ((at m (2, 1))/(at m (3, 1))) )

vector2DFromMatrix :: Matrix Double -> Vector2D
vector2DFromMatrix m = (V (at m (1, 1)) (at m (2, 1)) )

unitVector2DFromMatrix :: Matrix Double -> UnitVector2D
unitVector2DFromMatrix m = unitVector (V (at m (1, 1)) (at m (2, 1)) )

-- matrixFromFrame :: Frame -> Matrix Double
-- matrixFromFrame (FR (P ox oy) (UV ix iy) (UV jx jy))
--     = fromList [ [ ix,  jx, ox]
--                , [ iy,  jy, oy]
--                , [0.0, 0.0, 1.0]
--                ]


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
  geoMap :: Transformation -> a -> a

instance Coordinate Point2D where
  toPoint (P x y) = (realToFrac x, realToFrac y)
  x (P x _) = x
  y (P _ y) = y
  geoMap t (P x y) = (point2DFromMatrix $ times m v)
    where
      m = direct t -- matrixFromFrame frame
      v = fromList [ [x], [y], [1]]

instance Coordinate Vector2D where
  toPoint (V x y) = (realToFrac x, realToFrac y)
  x (V x _) = x
  y (V _ y) = y
  geoMap t (V x y) = (vector2DFromMatrix $ times m v)
    where
      m = direct t -- matrixFromFrame frame
      v = fromList [[x], [y], [0]]

instance Coordinate UnitVector2D where
  toPoint (UV x y) = (realToFrac x, realToFrac y)
  x (UV x _) = x
  y (UV _ y) = y
  geoMap t (UV x y) = (unitVector2DFromMatrix $ times m v)
    where
      m = direct t -- matrixFromFrame frame
      v = fromList [[x], [y], [0]]

-- ============================================================================

class GeoMapper a where
  invert:: a -> a
  compose :: a -> a -> a

geoMapCouple :: Coordinate a => Coordinate b => Frame -> (a,b) -> (a,b)
geoMapCouple frame (a,b) = ((geoMap frame a), (geoMap frame b))

geoMapTriple :: Coordinate a => Coordinate b => Coordinate c => Frame -> (a,b,c) -> (a,b,c)
geoMapTriple frame (a,b,c) = ((geoMap frame a), (geoMap frame b), (geoMap frame c))

-- invert :: Frame -> Maybe Frame
-- invert (FR (P ox oy) (UV ix iy) (UV jx jy)) =
--     case mm of
--       Just m -> Just (FR (homPoint $ col 3 m)
--                          (homUnitVector $ col 1 m)
--                          (homUnitVector $ col 2 m))
--       Nothing -> Nothing
--   where
--     mm = inv $ fromList
--         [ [ ix,  jx,  ox]
--         , [ iy,  jy,  oy]
--         , [0.0, 0.0, 1.0]
--         ]


-- ===============================================================================


instance GeoMapper Transformation where
  invert (TR direct inverse) = TR { direct = inverse, inverse = direct }
  compose (TR dir1 inv1) (TR dir2 inv2) =
    TR { direct = times dir1 dir2, inverse = times inv2 inv1 }

-- Rotation with angle in radians counterclockwise
rotation :: Double -> Transformation
rotation a = TR { direct = direct, inverse = inverse }
  where
    ca = cos a
    sa = sin a
    direct = fromList
      [ [  ca, (-sa), 0.0]
      , [  sa,    ca, 0.0]
      , [ 0.0,   0.0, 1.0]
      ]
    inverse = fromList
      [ [  ca,  sa, 0.0]
      , [(-sa), ca, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]


scale :: Double -> Double -> Transformation
scale sx sy = TR { direct = direct, inverse = inverse }
  where
    direct = fromList
      [ [  sx, 0.0, 0.0]
      , [ 0.0,  sy, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]
    inverse = fromList
      [ [  1/sx, 0.0, 0.0]
      , [ 0.0, 1/sy, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]

translation :: Double -> Double -> Transformation
translation tx ty = TR { direct = direct, inverse = inverse }
  where
    direct = fromList
      [ [ 1.0, 0.0, tx]
      , [ 0.0, 1.0, ty]
      , [ 0.0, 0.0, 1.0]
      ]
    inverse = fromList
      [ [ 1.0, 0.0, (-tx)]
      , [ 0.0, 1.0, (-ty)]
      , [ 0.0, 0.0, 1.0]
      ]

-- FRAME ==================================================================

type Frame = Transformation

-- class ReferenceFrame a where

-- The matrix for a frame represent a rigid transformation
-- [ M t ] => the inverse is [ MT  MT*t ]
-- [ 0 1 ]                   [  0     1 ]
-- =========================================================================
frame :: Point2D -> UnitVector2D -> UnitVector2D -> Frame
frame (P ox oy) (UV ix iy) (UV jx jy) =
    TR { direct = direct, inverse = inverse }
  where
    direct = fromList
        [ [ ix,  jx,  ox]
        , [ iy,  jy,  oy]
        , [0.0, 0.0, 1.0]
        ]
    inverse = fromList
        [ [ ix,  iy,  -(ix*ox+iy*oy)]
        , [ jx,  jy,  -(jx*ox+jy*oy)]
        , [0.0, 0.0, 1.0]
        ]

-- instance ReferenceFrame Frame where
origin:: Frame -> Point2D
origin f = (P ((at m (1, 3))/(at m (3, 3))) ((at m (2, 3))/(at m (3, 3))) )
  where
    m = direct f

iaxe :: Frame -> UnitVector2D
iaxe f = unitVector (V (at m (1, 1)) (at m (2, 1)) )
  where
    m = direct f

jaxe :: Frame -> UnitVector2D
jaxe f = unitVector (V (at m (1, 2)) (at m (2, 2)) )
  where
    m = direct f


