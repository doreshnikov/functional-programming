module Block4.Task1Spec
 ( stringSumTestTree
 ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

import Block4.Task1 (stringSum)

stringSumTestTree :: IO TestTree
stringSumTestTree = testSpec "stringSum" stringSumSpec

stringSumSpec :: Spec
stringSumSpec = do
  it "empty" $
    stringSum "" `shouldBe` Just 0
  it "one value" $
    stringSum "123" `shouldBe` Just 123
  it "many values" $
    stringSum "123 45 67" `shouldBe` Just (123 + 45 + 67)
  it "lots of spaces" $
    stringSum "  1  2  345 \t\n 67" `shouldBe` Just (1 + 2 + 345 + 67)
  it "only invalid" $
    stringSum "  x  " `shouldBe` Nothing
  it "invalid" $
    stringSum "1 2 3 $ 5 6 7" `shouldBe` Nothing
