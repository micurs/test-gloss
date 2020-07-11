module GeoSpec where

import Test.Hspec
import Geo

-- x : number you want rounded, n : number of decimal places you want...
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (round (x * t))) / t
    where t = 10^n

spec:: Spec
spec = do
  describe "Testing Geo.Base" $ do

    it "Basic zero Point" $
      zeroPoint `shouldBe` (P 0.0 0.0)

    it "Should create a unit vector" $
      let
        (UV ux uy) = unitVector2D (V 32.5 56.9)
        l = (ux*ux + uy*uy)
      in  (l<1.00001 && l>0.9999) `shouldBe` True

    it "Should create a unit vector from a list" $
      let
        (UV ux uy) = fromList [32.5, 56.9, 100.0] :: UnitVector2D
        l = (ux*ux + uy*uy)
      in (l<1.00001 && l>0.9999) `shouldBe` True

    it "Should create a vector from a list" $
      let
        v = fromList [2.0, 2.0, 2.0] :: Vector2D
      in do
        x v `shouldBe` 2.0
        y v `shouldBe` 2.0

    describe "Testing Geo.Base Point2D" $ do
      it "Should create a Point2D from a list" $
        let
          p = fromList [2.0, 2.0, 2.0] :: Point2D
        in do
          x p `shouldBe` 1.0
          y p `shouldBe` 1.0

      it "Should create a Point2D by adding a Vector2D to a Point2D" $
        let
          p = P 2.0 2.0
          v = V 1.0 2.0
          p' = p .-> v
        in do
          x p' `shouldBe` 3.0
          y p' `shouldBe` 4.0

      it "Should create a Gloss Point from a Point2D" $
        let
          p = P 3.0 4.0
          (px, py) = toPoint p
        in do
          px `shouldBe` 3.0
          py `shouldBe` 4.0

      it "With geoMap it should translate a Point2D" $
        let
          p = P 1.0 1.0
          t = translation 5.0 2.0
          p' = geoMap t p
        in do
          x p' `shouldBe` 6.0
          y p' `shouldBe` 3.0

      it "With geoMap it should rotate a Point2D" $
        let
          p = P 1.0 1.0
          t = rotation (pi/2)
          p' = geoMap t p
        in do
          truncate' (x p') 4 `shouldBe` (-1.0000)
          truncate' (y p') 4 `shouldBe` 1.0000

      it "With geoMap it should scale a Point2D" $
        let
          p = P 1.0 1.0
          t = scale 2.0 3.5
          p' = geoMap t p
        in do
          truncate' (x p') 4 `shouldBe` 2.0
          truncate' (y p') 4 `shouldBe` 3.5

      it "With geoMapBack it should translate a Point2D" $
        let
          p = P 6.0 3.0
          t = translation 5.0 2.0
          p' = geoMapBack t p
        in do
          x p' `shouldBe` 1.0
          y p' `shouldBe` 1.0

      it "With geoMapBack it should rotate a Point2D" $
        let
          p = P (-1.0) 1.0
          t = rotation (pi/2)
          p' = geoMapBack t p
        in do
          truncate' (x p') 4 `shouldBe` 1.0
          truncate' (y p') 4 `shouldBe` 1.0

      it "With geoMapBack it should scale a Point2D" $
        let
          p = P 2.0 3.5
          t = scale 2.0 3.5
          p' = geoMapBack t p
        in do
          truncate' (x p') 4 `shouldBe` 1.0
          truncate' (y p') 4 `shouldBe` 1.0
