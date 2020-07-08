{-# LANGUAGE TypeSynonymInstances #-}

module Geo
  ( module Geo.Basic
  , Coordinate(..)
  , Dottable(..)
  , Transformer(..)
  , baseFrame
  , vectorFromPoints
  , pointPlusVector
  , vectorPlusVector
  , orthoUnitVector
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

import Geo.Basic

-- ============================================================================




homPoint :: [Double] -> Point2D
homPoint [x,y,k] = P (x/k) (y/k)

homVector :: [Double] -> Vector2D
homVector [x,y,_] = V x y

homUnitVector:: [Double] -> UnitVector2D
homUnitVector [x,y,_] = UV x y

-- Mixed Operations


vectorFromPoints :: Point2D -> Point2D -> Vector2D
vectorFromPoints (P p1x p1y) (P p2x p2y) = V (p1x - p2x) (p1y - p2y)

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

orthoVector :: Vector2D -> Vector2D
orthoVector (V x y) = V y x

orthoUnitVector :: UnitVector2D -> UnitVector2D
orthoUnitVector (UV x y) = UV (-y) x


coordinatesFromMatrix :: Matrix Double -> (Double, Double, Double)
coordinatesFromMatrix m = ( at m (1, 1), at m (2, 1), at m (3, 1))

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
  geoMap :: Transformer t => t -> a -> a
  geoMapBack :: Transformer t => t -> a -> a

instance Coordinate Point2D where

  toPoint (P x y) = (realToFrac x, realToFrac y)

  x (P x _) = x

  y (P _ y) = y

  geoMap t (P x y) = P (tx/tk) (ty/tk)
    where
      (tx,ty,tk) = transform t (x,y,1.0)

  geoMapBack t (P x y) = P (tx/tk) (ty/tk)
    where
      (tx,ty,tk) = transformBack t (x,y,1.0)


instance Coordinate Vector2D where

  toPoint (V x y) = (realToFrac x, realToFrac y)

  x (V x _) = x

  y (V _ y) = y

  geoMap t (V x y) = V tx ty
    where
      (tx,ty,_) = transform t (x,y,0)

  geoMapBack t (V x y) = V tx ty
    where
      (tx,ty,_) = transformBack t (x,y,0.0)


instance Coordinate UnitVector2D where

  toPoint (UV x y) = (realToFrac x, realToFrac y)

  x (UV x _) = x

  y (UV _ y) = y

  geoMap tr (UV i j) = unitVector (V ti tj)
    where
      (ti,tj,_) = transform tr (i,j,0)

  geoMapBack t (UV i j) = unitVector (V ti tj)
    where
      (ti,tj,_) = transformBack t (i,j,0.0)

-- ============================================================================

class Transformer a where
  invert:: a -> a
  compose :: a -> a -> a
  (<<) :: a -> a -> a
  transform :: a -> (Double, Double, Double) -> (Double, Double, Double)
  transformBack :: a -> (Double, Double, Double) -> (Double, Double, Double)

geoMapCouple :: Coordinate a => Coordinate b => Frame -> (a,b) -> (a,b)
geoMapCouple frame (a,b) = ((geoMap frame a), (geoMap frame b))

geoMapTriple :: Coordinate a => Coordinate b => Coordinate c => Frame -> (a,b,c) -> (a,b,c)
geoMapTriple frame (a,b,c) = ((geoMap frame a), (geoMap frame b), (geoMap frame c))


-- ===============================================================================


instance Transformer Transformation where

  invert (TR direct inverse) =
    TR { direct = inverse, inverse = direct }

  compose (TR dir1 inv1) (TR dir2 inv2) =
    TR { direct = times dir1 dir2, inverse = times inv2 inv1 }

  (<<) a b = compose a b

  transform (TR direct _) (x,y,k) = coordinatesFromMatrix $ times direct v
    where
      v = fromList [[x], [y], [k]]

  transformBack (TR _ inverse) (x,y,k) = coordinatesFromMatrix $ times inverse v
    where
      v = fromList [[x], [y], [k]]

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

-- The matrix for a frame represent a rigid transformation
-- [ M t ] => the inverse is [ MT  MT*t ]
-- [ 0 1 ]                   [  0     1 ]
-- =========================================================================
frame :: Point2D -> UnitVector2D -> UnitVector2D -> Frame
frame (P ox oy) (UV ix iy) (UV jx jy) =
    TR { direct = direct, inverse = inverse }
  where
    inverse = fromList
        [ [ ix,  jx,  ox]
        , [ iy,  jy,  oy]
        , [0.0, 0.0, 1.0]
        ]
    direct = fromList
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

baseFrame :: Frame
baseFrame = frame (P 0 0) unitVectorX unitVectorY