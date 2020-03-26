module Block1.Task2Spec
 ( natTestTree
 ) where

import Control.Exception (evaluate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, describe, it, shouldBe, shouldNotBe, shouldThrow,
  testSpec)

import Block1.Task2 (Nat(..))

natTestTree :: IO TestTree
natTestTree = testSpec "Nat" natSpec

natSpec :: Spec
natSpec = do
  describe "(==)" $ do
    it "0 == 0" $
      zero `shouldBe` zero
    it "1 != 0" $
      one `shouldNotBe` zero
    it "1 != 2" $
      one `shouldNotBe` two

  describe "add" $ do
    it "1 + 0" $
      one + zero `shouldBe` one
    it "2 + 1" $
      two + one `shouldBe` three
    it "1 + 2" $
      one + two `shouldBe` three

  describe "mul" $ do
    it "1 * 0" $
      one * zero `shouldBe` zero
    it "0 * 2" $
      zero * two `shouldBe` zero
    it "3 * 1" $
      three * one `shouldBe` three
    it "2 * 2" $
      two * two `shouldBe` four

  describe "sub" $ do
    it "3 - 1" $
      three - one `shouldBe` two
    it "1 - 3" $
      one - three `shouldBe` zero
    it "4 - 2" $
      four - two `shouldBe` two

  describe "integer conversion" $ do
    it "fromInteger 4" $
      fromInteger 4 `shouldBe` four
    it "toInteger 4" $
      toInteger four `shouldBe` 4
    it "fromInteger 0" $
      fromInteger 0 `shouldBe` zero
    it "toInteger 0" $
      toInteger zero `shouldBe` 0
    it "fromInteger negative" $
      evaluate (fromInteger (-1) :: Nat) `shouldThrow` anyErrorCall

  describe "compare" $ do
    it "2 < 3" $
      two < three `shouldBe` True
    it "4 > 3" $
      four > three `shouldBe` True
    it "0 <= 1" $
      zero <= one `shouldBe` True
    it "1 == 1" $
      one == one `shouldBe` True
    it "4 >= 4" $
      four >= four `shouldBe` True

  describe "bonus" $ do
    it "even 1" $
      even one `shouldBe` False
    it "even 4" $
      even four `shouldBe` True
    it "3 div 2" $
      three `div` two `shouldBe` one
    it "4 mod 3" $
      four `mod` three `shouldBe` one
    it "1 div 0" $
      evaluate (one `div` zero) `shouldThrow` anyErrorCall

  where
    zero  = Z
    one   = S zero
    two   = S one
    three = S two
    four  = S three
