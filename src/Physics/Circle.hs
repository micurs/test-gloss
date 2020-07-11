module Physics.Circle
  ( Circle(..)
  , createCircle
  ) where

import Geo

data Circle = Circle
  { center :: Point2D
  , radius :: Double
  } deriving Show

createCircle :: Double -> Point2D -> Circle
createCircle r p = Circle
  { center = p
  , radius = r
  }

