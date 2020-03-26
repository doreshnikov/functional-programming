module Block3.Task1Spec
 ( maybeEitherConcatTestTree
 ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task1 (eitherConcat, maybeConcat)

maybeEitherConcatTestTree :: IO TestTree
maybeEitherConcatTestTree = testSpec "maybe/eitherConcat" maybeEitherConcatSpec

maybeEitherConcatSpec :: Spec
maybeEitherConcatSpec = do
  describe "maybeConcat" $ do
    it "empty list" $
      maybeConcat ([] :: [Maybe String]) `shouldBe` ""
    it "one Just" $
      maybeConcat [Just "x"] `shouldBe` "x"
    it "all Nothings" $
      maybeConcat ([Nothing, Nothing] :: [Maybe [Int]]) `shouldBe` []
    it "mixed" $
      maybeConcat [Just [1, 2, 3], Nothing, Just [2]] `shouldBe` [1, 2, 3, 2]

  describe "eitherConcat" $ do
    it "empty list" $
      eitherConcat ([] :: [Either String [Int]]) `shouldBe` ("", [])
    it "no lefts" $
      eitherConcat [Right [1, 2], Right [3, 4]] `shouldBe` ("", [1, 2, 3, 4])
    it "no rights" $
      eitherConcat ([Left "yz"] :: [Either String [Int]]) `shouldBe` ("yz", [])
    it "mixed" $
      eitherConcat [Left [1], Right "xy", Left [2]] `shouldBe` ([1, 2], "xy")
