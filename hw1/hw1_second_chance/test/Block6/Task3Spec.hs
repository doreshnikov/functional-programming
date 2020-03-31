module Block6.Task3Spec
 ( simpleParsersTestTree
 ) where

import Control.Applicative (empty, (<|>))
import Control.Exception (evaluate)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow, testSpec)

import Block6.Task1 (runParser)
import Block6.Task3 (CBS(..), cbsParser, intParser)
import Block6.Utils (extract)

simpleParsersTestTree :: IO TestTree
simpleParsersTestTree = testSpec "simple parser" simpleParsersSpec

simpleParsersSpec :: Spec
simpleParsersSpec = do
  describe "cbsParser" $ do
    it "<empty>" $
      extract (runParser cbsParser "") `shouldBe` e
    it "()" $
      extract (runParser cbsParser "()") `shouldBe` wrap e e
    it "(())(()())" $
      extract (runParser cbsParser "(())(()())") `shouldBe`
        wrap (wrap e e) (wrap (wrap e (wrap e e)) e)
    it "()) - fail" $
      evaluate (extract $ runParser cbsParser "())") `shouldThrow` anyErrorCall
    it "()( - fail" $
      evaluate (extract $ runParser cbsParser "()(") `shouldThrow` anyErrorCall

  describe "intParser" $ do
    it "<empty> - fail" $
      evaluate (extract $ runParser intParser "") `shouldThrow` anyErrorCall
    it "123" $
      extract (runParser intParser "123") `shouldBe` 123
    it "+123" $
      extract (runParser intParser "+123") `shouldBe` 123
    it "-123" $
      extract (runParser intParser "-123") `shouldBe` -123
    it "0" $
      extract (runParser intParser "0") `shouldBe` 0
    it "+0" $
      extract (runParser intParser "+0") `shouldBe` 0
    it "-0" $
      extract (runParser intParser "-0") `shouldBe` -0
    it "a123 - fail" $
      evaluate (extract $ runParser intParser "a123") `shouldThrow` anyErrorCall
    it "123a - residual" $
      runParser intParser "123a - residual" `shouldBe` Just (123, "a - residual")

    where
      e    = Empty
      wrap = WrapConcat
