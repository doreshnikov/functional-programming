module Task567.FileSystemSpec
  ( fileSystemTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow, testSpec)

import Lens.Micro

import Task567.FileSystem
import Task567.Practice

fileSystemTestTree :: IO TestTree
fileSystemTestTree = testSpec "file system spec" fileSystemSpec

fileSystemSpec :: Spec
fileSystemSpec = do
  describe "name" $ do
    it "name root" $
      root ^. name `shouldBe` "root"
    it "name a" $
      a ^. name `shouldBe` "a"
    it "name root ++ x" $
      (root & name %~ (++ "x")) `shouldBe` root {_name = "rootx"}
    it "name a ++ a" $
      (a & name %~ (++ "a")) `shouldBe` a {_name = "aa"}
  describe "contents" $ do
    it "contents root" $
      (root ^. contents) `shouldBe` _contents root
    it "contents' root" $
      (root ^.. contents') `shouldBe` [_contents root]
    it "contents' a" $
      (a ^.. contents') `shouldBe` []
  describe "prisms" $ do
      it "root _Dir" $
        has _Dir root `shouldBe` True
      it "a _Dir" $
        has _Dir a `shouldBe` False
      it "root _File" $
        has _File root `shouldBe` False
      it "a _File" $
        has _File a `shouldBe` True
  describe "practice" $ do
      it "listDir root" $
        listDir root `shouldBe` _contents root
      it "listDir a" $
        listDir a `shouldBe` []
      it "validateDir root" $
        validateDir root `shouldBe` Just "root"
      it "validateDir a" $
        validateDir a `shouldBe` Nothing
      it "validateFile root" $
        validateFile root `shouldBe` ""
      it "validateFile a" $
        validateFile a `shouldBe` "a"
      it "toRoot root" $
        _name (toRoot root) `shouldBe` "/"
      it "toRoot a" $
        _name (toRoot a) `shouldBe` "a"
      it "addSuffix root" $
        _name (addSuffix "x" root) `shouldBe` "rootx"
      it "addSuffix a" $
        _name (addSuffix "b" a) `shouldBe` "ab"
      it "firstDir root" $
        firstDir root `shouldBe` Just "xxx"
      it "firstDir a" $
        firstDir a `shouldBe` Nothing
      it "allFiles root" $
        allFiles root `shouldBe` ["a", "b"]
      it "allFiles a" $
        allFiles a `shouldBe` []
  where
    root =
      Dir
        "root"
        [ File "a"
        , Dir
            "xxx"
            [ File "b"
            , File "c"
            ]
        , File "b"
        , Dir
            "yyy"
            []
        ]
    a = head $ _contents root