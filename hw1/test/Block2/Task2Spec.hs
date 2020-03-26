module Block2.Task2Spec
 ( splitJoinTestTree
 ) where

import Data.List.NonEmpty (fromList)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldNotBe, testSpec)

import Block2.Task2 (joinWith, splitOn)

splitJoinTestTree :: IO TestTree
splitJoinTestTree = testSpec "split/join" splitJoinSpec

splitJoinSpec :: Spec
splitJoinSpec = do
  describe "splitOn" $ do
    it "path/to/file" $
      splitOn '/' "path/to/file" `shouldBe` fromList ["path", "to", "file"]
    it "<empty>" $
      splitOn '/' "" `shouldBe` fromList [""]
    it "repeated xxxx" $
      splitOn 'x' "axxxxb" `shouldBe` fromList ["a", "", "" , "", "b"]

  describe "joinWith" $ do
    it "[\"path\", \"to\", \"file\"]" $
      joinWith '|' (fromList ["path", "to", "file"]) `shouldBe` "path|to|file"
    it "<empty>" $
      joinWith '!' (fromList [""]) `shouldBe` ""
    it "repeated in result" $
      joinWith 'y' (fromList ["a", "", "" , "", "b"]) `shouldBe` "ayyyyb"
