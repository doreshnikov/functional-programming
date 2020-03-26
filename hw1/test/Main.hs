module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Block1.Task1Spec (weekdayTestTree)
import Block1.Task2Spec (natTestTree)
import Block1.Task3Spec (treeTestTree)

main :: IO ()
main = do
  b1t1 <- weekdayTestTree
  b1t2 <- natTestTree
  b1t3 <- treeTestTree
  defaultMain $ testGroup "Block1" [b1t1, b1t2, b1t3]
