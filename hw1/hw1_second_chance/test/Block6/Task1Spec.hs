module Block6.Task1Spec
 ( parserInstancesTestTree
 ) where

import Control.Applicative (empty, (<|>))
import Control.Exception (evaluate)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
  (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow, testSpec)

import Block6.Task1 (Parser(..))
import Block6.Utils (extract)

parserInstancesTestTree :: IO TestTree
parserInstancesTestTree = testSpec "parser instances" parserInstancesSpec

parserInstancesSpec :: Spec
parserInstancesSpec = do
  describe "Functor" $ do
    it "fmap" $
      extract (runParser ((:) <$> (pure 1)) "") [2, 3] `shouldBe` [1, 2, 3]

  describe "Applicative" $ do
    it "pure" $
      extract (runParser (pure 1) "") `shouldBe` 1
    it "(<*>)" $
      extract (runParser ((:) <$> (pure 1) <*> (pure [2])) "") `shouldBe` [1, 2]

  describe "Monad" $ do
    it ">>=" $
      extract (runParser ((pure 1) >>= \x -> return (x + 1)) "") `shouldBe` 2

  describe "Alternative" $ do
    it "empty" $
      evaluate (extract $ runParser empty " ") `shouldThrow` anyErrorCall
    it "<|>" $
      extract (runParser (empty <|> pure 1) " ") `shouldBe` 1
