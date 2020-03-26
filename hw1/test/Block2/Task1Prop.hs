module Block2.Task1Prop
 ( treeFoldableTestTree
 ) where

import Data.Foldable (toList)
import Data.List (sort)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Block1.Task3 (fromList)
import Block1.Task3Spec (V(..))
import Block2.Task1 (Tree(..))

treeFoldableTestTree :: IO TestTree
treeFoldableTestTree = return $
  testGroup "Tree: toList . fromList === id" [ testProperty "with Int" propIntFold
                                             , testProperty "with V" propVFold
                                             ]

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 100000) Gen.enumBounded

genVList :: Gen [V]
genVList = Gen.list (Range.linear 0 100000) (Gen.element [X, Y, Z])

propIntFold :: Property
propIntFold = property $ do
  lInt <- forAll genIntList
  (toList $ fromList lInt) === (sort lInt)

propVFold :: Property
propVFold = property $ do
  lV <- forAll genVList
  (toList $ fromList lV) === (sort lV)
