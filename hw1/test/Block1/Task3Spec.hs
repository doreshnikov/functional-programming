module Block1.Task3Spec
 ( V(..)
 , treeTestTree
 ) where

import Data.List.NonEmpty (NonEmpty(..))
import Numeric.Natural (Natural)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldNotBe, testSpec)

import Block1.Task3 (Tree(..), empty, find, fromList, insert, remove, size)

data V = X | Y | Z deriving Show

instance Eq V where
  (==) X Y = True
  (==) Y X = True
  (==) X X = True
  (==) Y Y = True
  (==) Z Z = True
  (==) _ _ = False

instance Ord V where
  (<=) Z X = False
  (<=) Z Y = False
  (<=) _ _ = True

treeTestTree :: IO TestTree
treeTestTree = testSpec "Tree" treeSpec

treeSpec :: Spec
treeSpec = do
  describe "(==)" $ do
    it "Leafs" $
      (Leaf :: Tree Int) `shouldBe` Leaf
    it "Leaf != Branch" $
      (Leaf :: Tree Int) `shouldNotBe` two
    it "two == two" $
      two `shouldBe` two
    it "example == example" $
      example `shouldBe` example
    it "two != example" $
      two `shouldNotBe` example
    it "1 != 2" $
      single 1 `shouldNotBe` two
    it "X == Y" $
      single X `shouldBe` single Y

  describe "empty" $ do
    it "empty Leaf" $
      empty Leaf `shouldBe` True
    it "empty Branch" $
      empty example `shouldBe` False

  describe "find" $ do
    it "find in Leaf" $
      find Leaf 1 `shouldBe` Nothing
    it "find in singleton (ok)" $
      find two 2 `shouldBe` Just 2
    it "find in singleton (fail)" $
      find two 1 `shouldBe` Nothing
    it "find in branch (ok)" $
      find example 3 `shouldBe` Just 3
    it "find in branch (fail)" $
      find example 4 `shouldBe` Nothing
    it "find in branch (hard)" $
      find nontriv Y `shouldBe` Just X

  describe "insert" $ do
    it "[2]" $
      fromList [2] `shouldBe` two
    it "[2,2,1,3]" $
      fromList [2, 2, 1, 3] `shouldBe` example
    it "[2,1,3,2]" $
      fromList [2, 1, 3, 2] `shouldBe` example
    it "[X,Y,Z,Z]" $
      fromList [X, Y, Z, Z] `shouldBe` nontriv
    it "[Z,X,Y,Z] (fail)" $
      fromList [Z, X, Y, Z] `shouldNotBe` nontriv

  describe "insert" $ do
    it "insert into Leaf" $
      find (insert Leaf 1) 1 `shouldBe` Just 1
    it "insert 2,1,3 into [2]" $
      foldl insert two [2, 1, 3] `shouldBe` example
    it "insert & find 4" $
      find (insert example 4) 4 `shouldBe` Just 4
    it "insert & find Y in [X] (hard)" $
      find (insert (single X) Y) Y `shouldBe` Just X

  describe "remove" $ do
    it "remove from Leaf" $
      remove Leaf 1 `shouldBe` Leaf
    it "remove 2 from [2]" $
      remove two 2 `shouldBe` Leaf
    it "remove 1 from [1,2,2,3]" $
      find (remove example 1) 1 `shouldBe` Nothing
    it "remove 2 from [1,2,2,3]" $
      find (remove example 2) 2 `shouldBe` Just 2
    it "remove 3 from [1,2,2,3]" $
      find (remove example 3) 3 `shouldBe` Nothing
    it "remove 4 from [1,2,2,3]" $
      remove example 4 `shouldBe` example
    it "remove Y (hard)" $
      find (remove nontriv Y) X `shouldBe` Just Y

  describe "remove (big)" $ do
    it "size (ok) 1" $
      size fulln0 `shouldBe` (size full) - 1
    it "size (ok) 2" $
      size fulln1 `shouldBe` (size full) - 1
    it "size (ok) 3" $
      size fulln1' `shouldBe` (size full) - 1
    it "find 0 in full \\ 0" $
      find fulln0 0 `shouldBe` Nothing
    it "find rmin in full \\ 0" $
      find fulln0 1 `shouldBe` Just 1
    it "find 4 in full \\ 4" $
      find fulln1 4 `shouldBe` Nothing
    it "find rmin in full \\ 4" $
      find fulln1 5 `shouldBe` Just 5
    it "find -4 in full \\ -4" $
      find fulln1' (-4) `shouldBe` Nothing
    it "find rmin in full \\ -4" $
      find fulln1' (-3) `shouldBe` Just (-3)

  describe "size" $ do
    it "size single" $
      size two `shouldBe` 1
    it "size example" $
      size example `shouldBe` 4
    it "size (hard)" $
      size nontriv `shouldBe` 4
    it "size (full)" $
      size full `shouldBe` 15

  where
    single :: a -> Tree a
    single x = Branch (x :| []) Leaf Leaf

    two     = single 2
    example = Branch (2 :| [2]) (single 1) (single 3)
    nontriv = Branch (X :| [Y]) Leaf (Branch (Z :| [Z]) Leaf Leaf)

    big :: Natural -> Int -> Tree Int
    big 0 _ = Leaf
    big x v = Branch (v :| []) (big (x - 1) (v - dv)) (big (x - 1) (v + dv))
      where
        dv = 2 ^ (x - 2)

    full    = big 4 0
    fulln0  = remove full 0
    fulln1  = remove full 4
    fulln1' = remove full (-4)
