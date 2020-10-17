module Main where

import Test.Tasty (defaultMain)

import Spec

main :: IO ()
main = do
  test <- fsTestTree
  defaultMain test