module Task1.Crit
  ( geometryCompareBench
  , geometryMaxtestBench,
  ) where

import Control.DeepSeq (deepseq)
import Criterion.Main (Benchmark, bench, bgroup, nf, whnf)

import           Task1.Fast
import qualified Task1.Naive as N

geometryCompareBench :: Benchmark
geometryCompareBench =
  bgroup
    "geometry compare"
    [ bgroup
        "perimeter 5000"
        [ np `deepseq` bench "naive" $ nf N.perimeter np
        , p `deepseq` bench "fast" $ nf perimeter p
        ]
    , bgroup
        "double area 5000"
        [ np `deepseq` bench "naive" $ nf N.doubleArea np
        , p `deepseq` bench "fast" $ nf doubleArea p
        ]
    , bgroup
        "perimeter 50000"
        [ npx `deepseq` bench "naive" $ nf N.perimeter npx
        , px `deepseq` bench "fast" $ nf perimeter px
        ]
    , bgroup
        "double area 50000"
        [ npx `deepseq` bench "naive" $ nf N.doubleArea npx
        , px `deepseq` bench "fast" $ nf doubleArea px
        ]
    ]
  where
    np  = replicate 5000  $ N.Point 1 2
    p   = replicate 5000  $ Point 1 2
    npx = replicate 50000 $ N.Point 1 2
    px  = replicate 50000 $ Point 1 2

geometryMaxtestBench :: Benchmark
geometryMaxtestBench =
  bgroup
    "geometry maxtest"
    [ p `deepseq` bench "perimeter 10^7" $ whnf perimeter p
    , p `deepseq` bench "double area 10^7" $ whnf doubleArea p
    ]
  where
    p = replicate 10000000 $ Point 1000000 10000000
