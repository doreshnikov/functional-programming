{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Task1.Naive
-- Description : Naive implementation of Geometry evaluations
--
-- Point data type and other geometric primitives (only for benchmarking purposes).
module Task1.Naive
  ( -- * Data types
    Point (..)

    -- * Functions
  , doubleArea
  , perimeter
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Data type representing a point with integer coordinates.
data Point = Point
  { -- | An x-coordinate.
    x :: Int,
    -- | An y-coordinate.
    y :: Int
  }
  deriving (Eq, Read, Show, Generic, NFData)

coordinateWise :: (Int -> Int -> Int) -> (Int -> Int -> t) -> (Point -> Point -> t)
coordinateWise op trans Point {x = x1, y = y1} Point {x = x2, y = y2} =
  trans (op x1 x2) (op y1 y2)

minus :: Point -> Point -> Point
minus = coordinateWise (-) Point

norm :: Point -> Double
norm Point {..} = sqrt . fromIntegral $ x * x + y * y

pairWiseSum :: (Num t) => (Point -> Point -> t) -> [Point] -> t
pairWiseSum _ [] = 0
pairWiseSum op s@(x : _) = pairWiseSum' op s x
  where
    pairWiseSum' :: (Num t) => (Point -> Point -> t) -> [Point] -> Point -> t
    pairWiseSum' _ [] _              = 0
    pairWiseSum' f [b] a             = f b a
    pairWiseSum' f (b : q@(c : _)) a = f b c + pairWiseSum' f q a

-- | Perimeter of an anticlockwise-oriented polygon.
perimeter :: [Point] -> Double
perimeter = pairWiseSum (\a b -> norm $ minus a b)

-- | Double area of an anticlockwise-oriented polygon.
doubleArea :: [Point] -> Int
doubleArea = abs . pairWiseSum (\a b -> (x b - x a) * (y b + y a))
