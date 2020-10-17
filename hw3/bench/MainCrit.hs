module Main
  ( main
  ) where

import Criterion.Main (defaultMain)

import Task1.Crit

main :: IO ()
main =
  defaultMain
    [ geometryCompareBench
    , geometryMaxtestBench
    ]
