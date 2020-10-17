module Task1.GeometrySpec
  ( geometryTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Task1.Fast

geometryTestTree :: IO TestTree
geometryTestTree = testSpec "geometry spec" geometrySpec

geometrySpec :: Spec
geometrySpec = do
  describe "rotate90" $ do
    it "0 0 -> 0 0" $
      rotate90 o `shouldBe` o
    it "3 2 -> 2 -3" $
      rotate90 a `shouldBe` Point 2 (-3)
  describe "plus" $ do
    it "0 0 + 3 2 -> 3 2" $
      plus o a `shouldBe` a
    it "3 2 + -5 1 -> -2 3" $
      plus a b `shouldBe` Point (-2) 3
  describe "minus" $ do
    it "0 0 - 3 2 -> -3 -2" $
      minus o a `shouldBe` Point (-3) (-2)
    it "3 2 - -5 1 -> 8 1" $
      minus a b `shouldBe` Point 8 1
  describe "scalar" $ do
    it "0 0 . 3 2 -> 0" $
      scalarProduct o a `shouldBe` 0
    it "3 2 . -5 1 -> -13" $
      scalarProduct a b `shouldBe` -13
  describe "cross" $ do
    it "0 0 x 3 2 -> 0" $
      crossProduct o a `shouldBe` 0
    it "3 2 x -5 1 -> 13" $
      crossProduct a b `shouldBe` 13
  describe "norm" $ do
    it "0 0 -> 0" $
      norm o `shouldBe` 0
    it "3 2 -> -|13" $
      norm a `shouldBe` sqrt 13
  describe "perimeter" $ do
    it "oab" $
      perimeter [o, a, b] `shouldBe` sqrt 13 + sqrt 26 + sqrt 65
  describe "double area" $ do
    it "oab" $
      doubleArea [o, a, b] `shouldBe` 13
  where
    o = Point 0 0
    a = Point 3 2
    b = Point (-5) 1