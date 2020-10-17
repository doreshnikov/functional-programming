module Main
  ( main
  ) where

import Test.Tasty (defaultMain, testGroup)

import Task1.GeometrySpec (geometryTestTree)

import Task567.FileSystemSpec (fileSystemTestTree)
import Task567.VisitorsSpec (visitorsTestTree)

main :: IO ()
main = do
  t1 <- geometryTestTree
  t5 <- fileSystemTestTree
  t6 <- visitorsTestTree
  defaultMain $ testGroup "All" [t1, t5, t6]
