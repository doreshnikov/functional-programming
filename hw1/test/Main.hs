module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Block1.Task1Spec (weekdayTestTree)
import Block1.Task2Spec (natTestTree)
import Block1.Task3Spec (treeTestTree)

import Block2.Task1Prop (treeFoldableTestTree)
import Block2.Task2Spec (splitJoinTestTree)

main :: IO ()
main = do
  b1t1 <- weekdayTestTree
  b1t2 <- natTestTree
  b1t3 <- treeTestTree

  b2t1 <- treeFoldableTestTree
  b2t2 <- splitJoinTestTree

  defaultMain $ testGroup "All" [ testGroup "Block1" [b1t1, b1t2, b1t3]
                                , testGroup "Block2" [b2t1, b2t2]
                                ]
