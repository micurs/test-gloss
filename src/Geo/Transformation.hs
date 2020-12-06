module Geo.Transformation
  ( Transformation
  , rotation
  , scale
  , translation
  , compose
  , (<<)
  , Frame
  , baseFrame
  , frame
  , geoMapCouple
  , geoMapTriple
  , frameTransformation
  ) where

import qualified Numeric.Matrix as M

import Geo.Base

data Transformation = TR
  { direct :: M.Matrix Double
  , inverse :: M.Matrix Double
  } deriving Show

newtype Frame = Frame Transformation deriving Show

coordinatesFromMatrix :: M.Matrix Double -> (Double, Double, Double)
coordinatesFromMatrix m = ( M.at m (1, 1), M.at m (2, 1), M.at m (3, 1))

-- =================== Transformation ====================================================

instance Transformer Transformation where
  directM m = direct m
  inverseM m = inverse m
  invert (TR direct inverse) =
    TR { direct = inverse, inverse = direct }
  transform (TR direct _) (x,y,k) = coordinatesFromMatrix $ M.times direct v
    where
      v = M.fromList [[x], [y], [k]]
  transformBack (TR _ inverse) (x,y,k) = coordinatesFromMatrix $ M.times inverse v
    where
      v = M.fromList [[x], [y], [k]]

compose :: Transformer a => Transformer b => a -> b -> Transformation
compose t1 t2 =
  TR { direct = M.times dir1 dir2, inverse = M.times inv2 inv1 }
  where
    dir1 = directM t1
    inv1 = inverseM t1
    dir2 = directM t2
    inv2 = inverseM t2

(<<) :: Transformer a => Transformer b => a -> b -> Transformation
(<<) a b = compose a b


rotation :: Double -> Transformation
rotation a = TR { direct = direct, inverse = inverse }
  where
    ca = cos a
    sa = sin a
    direct = M.fromList
      [ [  ca, (-sa), 0.0]
      , [  sa,    ca, 0.0]
      , [ 0.0,   0.0, 1.0]
      ]
    inverse = M.fromList
      [ [  ca,  sa, 0.0]
      , [(-sa), ca, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]

scale :: Double -> Double -> Transformation
scale sx sy = TR { direct = direct, inverse = inverse }
  where
    direct = M.fromList
      [ [  sx, 0.0, 0.0]
      , [ 0.0,  sy, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]
    inverse = M.fromList
      [ [  1/sx, 0.0, 0.0]
      , [ 0.0, 1/sy, 0.0]
      , [ 0.0, 0.0, 1.0]
      ]

translation :: Double -> Double -> Transformation
translation tx ty = TR { direct = direct, inverse = inverse }
  where
    direct = M.fromList
      [ [ 1.0, 0.0, tx]
      , [ 0.0, 1.0, ty]
      , [ 0.0, 0.0, 1.0]
      ]
    inverse = M.fromList
      [ [ 1.0, 0.0, (-tx)]
      , [ 0.0, 1.0, (-ty)]
      , [ 0.0, 0.0, 1.0]
      ]

-- ======================= Frame =========================================================

instance Transformer Frame where
  directM (Frame t) = directM t
  inverseM (Frame t) = inverseM t
  invert (Frame t) = Frame (invert t)
  -- compose (Frame t1) (Frame t2) = compose t1 t2
  -- (<<) (Frame t1) (Frame t2) = compose t1 t2
  transform (Frame t) c = transform t c
  transformBack (Frame t) c = transformBack t c

instance RefFrame Frame where
  origin f = (P x y)
    where
      m = directM f
      x = (M.at m (1, 3))/(M.at m (3, 3))
      y = (M.at m (2, 3))/(M.at m (3, 3))
  xAxe f = unitVector2D (V (M.at m (1, 1)) (M.at m (2, 1)) )
    where
      m = directM f
  yAxe f = unitVector2D (V (M.at m (1, 2)) (M.at m (2, 2)) )
    where
      m = directM f

frameTransformation :: Frame -> Transformation
frameTransformation f = TR { direct = d, inverse = i}
  where
    d = directM f
    i = inverseM f

baseFrame :: Frame
baseFrame = frame (P 0 0) unitVector2DX

frame :: Point2D -> UnitVector2D -> Frame
frame (P ox oy) (UV ix iy) =
    Frame TR { direct = direct, inverse = inverse }
  where
    jx = -iy
    jy = ix
    inverse = M.fromList
        [ [ ix,  jx,  ox]
        , [ iy,  jy,  oy]
        , [0.0, 0.0, 1.0]
        ]
    direct = M.fromList
        [ [ ix,  iy,  -(ix*ox+iy*oy)]
        , [ jx,  jy,  -(jx*ox+jy*oy)]
        , [0.0, 0.0, 1.0]
        ]

-- ============================================================================


geoMapCouple :: WithCoordinates a => WithCoordinates b => Transformation -> (a,b) -> (a,b)
geoMapCouple frame (a,b) = ((geoMap frame a), (geoMap frame b))

geoMapTriple :: WithCoordinates a => WithCoordinates b => WithCoordinates c => Transformation -> (a,b,c) -> (a,b,c)
geoMapTriple frame (a,b,c) = ((geoMap frame a), (geoMap frame b), (geoMap frame c))
