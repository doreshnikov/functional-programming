module Task567.VisitorsSpec
  ( visitorsTestTree,
  )
where

import Lens.Micro
import Task567.FileSystem
import Task567.Visitors
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldNotBe, testSpec)

visitorsTestTree :: IO TestTree
visitorsTestTree = do
  sSpec <- testSpec "visitors 6" simpleVisitorsSpec
  aSpec <- testSpec "visitors 7" advancedVisitorsSpec
  mSpec <- testSpec "move" moveSpec
  return $ testGroup "fs advanced" [sSpec, aSpec, mSpec]

root :: FS
root =
  Dir
    "root"
    [ File "a.txt",
      Dir
        "xxx"
        [ File "b.pdf",
          File "c.exe"
        ],
      File "b",
      Dir
        "yyy"
        []
    ]

a :: FS
a = head $ _contents root

xxx :: FS
xxx = _contents root !! 1

simpleVisitorsSpec :: Spec
simpleVisitorsSpec = do
  describe "cd" $ do
    it "root -> cd a.txt" $
      root ^? cd "a.txt" `shouldBe` Nothing
    it "root -> cd xxx" $
      root ^? cd "xxx" `shouldBe` Just xxx
    it "a.txt -> cd f" $
      a ^? cd "f" `shouldBe` Nothing
  describe "ls" $ do
    it "root -> ls" $
      root ^.. ls `shouldBe` ["a.txt", "xxx", "b", "yyy"]
    it "a.txt -> ls" $
      a ^.. ls `shouldBe` []
  describe "file" $ do
    it "root -> file a.txt" $
      root ^? file "a.txt" `shouldBe` Just a
    it "root -> file xxx" $
      root ^? file "xxx" `shouldBe` Nothing
    it "a.txt -> file f" $
      a ^? file "f" `shouldBe` Nothing
  describe "chained" $ do
    it "/ -> cd root -> cd xxx -> file b.pdf" $
      Dir "/" [root] ^? cd "root" . cd "xxx" . file "b.pdf" `shouldNotBe` Nothing
    it "/ -> cd root -> cd yyy -> ls" $
      Dir "/" [root] ^.. cd "root" . cd "xxx" . ls `shouldBe` ["b.pdf", "c.exe"]

advancedVisitorsSpec :: Spec
advancedVisitorsSpec = do
  describe "extension lens" $ do
    it "root" $
      root ^. extension `shouldBe` ""
    it "a.txt" $
      a ^. extension `shouldBe` ".txt"
    it "a.txt -> a.dat" $
      (a & extension .~ "dat") `shouldBe` File "a.dat"
  describe "replace extensions" $ do
    it "root -> .dat" $
      replaceExtensionsAt root ".dat"
        `shouldBe` Dir
          "root"
          [ File "a.dat",
            Dir
              "xxx"
              [ File "b.pdf",
                File "c.exe"
              ],
            File "b.dat",
            Dir
              "yyy"
              []
          ]
    it "a.txt -> .dat" $
      replaceExtensionsAt a ".dat" `shouldBe` a
  describe "list recursive lens" $ do
    it "root" $
      root ^.. listRecursive . name `shouldBe` ["root", "a.txt", "xxx", "b.pdf", "c.exe", "b", "yyy"]
    it "a.txt" $
      (a ^.. listRecursive) `shouldBe` [a]
  describe "list dir recursive" $ do
    it "root" $
      listDirRecursive root `shouldBe` ["root", "a.txt", "xxx", "b.pdf", "c.exe", "b", "yyy"]
    it "a.txt" $
      listDirRecursive a `shouldBe` []
  describe "remove empty" $ do
    it "root -> rm xxx" $
      removeEmptyDir root "xxx" `shouldBe` root'
    it "root -> rm yyy" $
      (removeEmptyDir root "yyy" ^.. ls) `shouldBe` ["a.txt", "b", "xxx"]
    it "root -> rm a.txt" $
      removeEmptyDir root "a.txt" `shouldBe` root'
  where
    root' :: FS
    root' =
      Dir
        "root"
        [ File "a.txt",
          File "b",
          Dir
            "xxx"
            [ File "b.pdf",
              File "c.exe"
            ],
          Dir
            "yyy"
            []
        ]

moveSpec :: Spec
moveSpec = do
  it "root -> a.txt" $ root ^? move "a.txt" . name `shouldBe` Just "root/a.txt"
  it "root -> xxx -> c.exe" $ root ^? move "xxx" . move "c.exe" . name `shouldBe` Just "root/xxx/c.exe"
  it "root -> notexists" $ root ^? move "notexists" `shouldBe` Nothing
  it "root -> yyy" $ root ^? move "yyy" . name `shouldBe` Just "root/yyy"
