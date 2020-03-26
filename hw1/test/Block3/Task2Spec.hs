module Block3.Task2Spec
 ( monoidsCheckTestTree
 ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Expectation, Spec, describe, it, shouldBe, testSpec)

import Block3.Task2 (Endo(..), Name(..), NonEmpty(..), ThisOrThat(..))

monoidsCheckTestTree :: IO TestTree
monoidsCheckTestTree = testSpec "Semigroups and Monoids" monoidsCheckSpec

monoidsCheckSpec :: Spec
monoidsCheckSpec = do
  describe "NonEmpty" $ do
    it "one <> one" $
      1 :| [] <> 2 :| [] `shouldBe` 1 :| [2]
    it "many <> many" $
      1 :| [2, 3] <> 4 :| [5, 6] `shouldBe` 1 :| [2, 3, 4, 5, 6]
    it "assoc" $
      assoc (1 :| [2]) (3 :| [4]) (5 :| [6])

  describe "ThisOrThat" $ do
    it "assoc 1" $
      assoc (This 1) (That 2) (Both 3 4)
    it "assoc 2" $
      assoc (That 1) (Both 2 3) (This 4)
    it "assoc 3" $
      assoc (Both 1 2) (This 3) (Both 5 6)
    it "assoc 4" $
      assoc (Both 1 2) (That 3) (This 4)

  describe "Name" $ do
    it "example" $
      Name "root" <> Name "server" `shouldBe` Name "root.server"
    it "concat with mempty" $
      Name "root" <> mempty `shouldBe` Name "root"
    it "assoc" $
      assoc (Name "x") (Name "y") (Name "z")
    it "assoc with mempty" $
      assoc (Name "x") mempty (Name "z")

  describe "Endo" $ do
    it "assoc" $
      fassoc (Endo (+1)) (Endo (+2)) (Endo (+3)) 1
    it "assoc with mempty" $
      fassoc (Endo $ (++) "x") mempty (Endo $ (++) "y") "z"

  where
    assoc x y z    = shouldBe (x <> (y <> z)) ((x <> y) <> z)

    fassoc :: (Show a, Eq a) => Endo a -> Endo a -> Endo a -> a -> Expectation
    fassoc f g h x = shouldBe (getEndo (f <> (g <> h)) $ x)
                              (getEndo ((f <> g) <> h) $ x)
