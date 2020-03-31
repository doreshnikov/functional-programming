module Block6.Task2Spec
 ( primitiveCombinatorsTestTree
 ) where

import Control.Applicative (empty, (<|>))
import Control.Exception (evaluate)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow, testSpec)

import Block6.Task1 (runParser)
import Block6.Task2 (anyP, element, eof, maybeP, ok, satisfy, stream)
import Block6.Utils

primitiveCombinatorsTestTree :: IO TestTree
primitiveCombinatorsTestTree = testSpec "primitive combinators" primitiveCombinatorsSpec

primitiveCombinatorsSpec :: Spec
primitiveCombinatorsSpec = do
  describe "ok" $ do
    it "should not fail" $
      extract (runParser ok "") `shouldBe` ()
    it "should not consume" $
      remains (runParser ok [1, 2]) `shouldBe` [1, 2]
  describe "eof" $ do
    it "should not fail (empty)" $
      extract (runParser eof "") `shouldBe` ()
    it "should fail (non-empty)" $
      evaluate (extract $ runParser eof [1, 2]) `shouldThrow` anyErrorCall
  describe "satisfy" $ do
    it "should fail (empty)" $
      evaluate (extract $ runParser (satisfy isNaN) []) `shouldThrow` anyErrorCall
    it "should fail (not satisfied)" $
      evaluate (extract $ runParser (satisfy isNaN) [1]) `shouldThrow` anyErrorCall
    it "should return (satisfied)" $
      runParser (satisfy $ \_ -> True) [1, 2] `shouldBe` Just (1, [2])
  describe "element" $ do
    it "should fail (not equal)" $
      evaluate (extract $ runParser (element 1) [2]) `shouldThrow` anyErrorCall
    it "should return (equal)" $
      runParser (element 1) [1, 2] `shouldBe` Just (1, [2])
  describe "stream" $ do
    it "should fail (not equal)" $
      evaluate (extract $ runParser (stream [2, 3]) [2, 4]) `shouldThrow` anyErrorCall
    it "should return (equal)" $
      runParser (stream [2, 3]) [2, 3, 4] `shouldBe` Just ([2, 3], [4])
  describe "anyP" $ do
    it "should fail (none)" $
      evaluate (extract $ runParser (anyP $ map element [1, 2]) [3]) `shouldThrow` anyErrorCall
    it "should return (match)" $
      runParser (anyP $ map element [2, 3]) [3, 4] `shouldBe` Just (3, [4])
  describe "maybeP" $ do
    it "should not fail" $
      runParser (maybeP $ element 1) [3] `shouldBe` Just (Nothing, [3])
    it "should return (match)" $
      runParser (maybeP $ element 1) [1, 3] `shouldBe` Just (Just 1, [3])
