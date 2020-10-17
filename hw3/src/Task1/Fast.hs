{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Task1.Strict
-- Description : Strict implementation of Geometry evaluations
--
-- Point data type and other geometric primitives.
module Task1.Fast
  ( -- * Data types
    Point (..)

    -- * Support operators
  , (!$)
  , (!!$)

    -- * Functions
  , crossProduct
  , doubleArea
  , minus
  , norm
  , pairWiseSum
  , perimeter
  , plus
  , rotate90
  , scalarProduct
  ) where

import Control.DeepSeq (($!!), NFData)
import GHC.Generics (Generic)

-- | Data type representing a point with integer coordinates.
data Point = Point
  { -- | An @unpack@ed strictly-evaluated x-coordinate.
    x :: {-# UNPACK #-} !Int,
    -- | An @unpack@ed strictly-evaluated y-coordinate.
    y :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Read, Show, Generic, NFData)

infixl 0 !$

-- | An operator similar to '($!)' but with left associativity.
(!$) :: (a -> b) -> a -> b
(!$) = ($!)

infixl 0 !!$

-- | An operator similar to '($!!)' but with left associativity.
(!!$) :: NFData a => (a -> b) -> a -> b
(!!$) = ($!!)

-- | Right 90 degrees rotation for a point's radius-vector.
rotate90 :: Point -> Point
rotate90 Point {..} = Point y (- x)

-- | Coordinate-wise addition of two points.
plus :: Point -> Point -> Point
plus Point {x = x1, y = y1} Point {x = x2, y = y2} =
  Point {x = x1 + x2, y = y1 + y2}

-- | Coordinate-wise subtraction of two points.
minus :: Point -> Point -> Point
minus Point {x = x1, y = y1} Point {x = x2, y = y2} =
  Point {x = x1 - x2, y = y1 - y2}

-- | Scalar product of two points.
scalarProduct :: Point -> Point -> Int
scalarProduct Point {x = x1, y = y1} Point {x = x2, y = y2} =
  x1 * x2 + y1 * y2

-- | Pseudo-scalar (integer cross) product of two points.
crossProduct :: Point -> Point -> Int
crossProduct a b = scalarProduct a $!! rotate90 b

-- | A point's radius-vector 2-norm (Pythagorean length).
norm :: Point -> Double
norm Point {x = !x, y = !y} = sqrt . fromIntegral $! x * x + y * y

-- | Pair-wise mapping and addition, Applies it's first argument to each
-- pair of adjacent points and then adds together all the results.
pairWiseSum :: (Num t, NFData t) => (Point -> Point -> t) -> [Point] -> t
pairWiseSum _ [] = 0
pairWiseSum !op s@(x : _) = pairWiseSum' op s x
  where
    pairWiseSum' :: (Num t, NFData t) => (Point -> Point -> t) -> [Point] -> Point -> t
    pairWiseSum' _  [] _             = 0
    pairWiseSum' f [b] a             = f b a
    pairWiseSum' f (b : q@(c : _)) a = (+) !$ f b c !$ pairWiseSum' f q a

-- | Perimeter of an anticlockwise-oriented polygon.
perimeter :: [Point] -> Double
perimeter = pairWiseSum (\a b -> norm $! minus a b)

-- | Double area of an anticlockwise-oriented polygon.
doubleArea :: [Point] -> Int
doubleArea p = abs $! pairWiseSum (\a b -> (x b - x a) * (y b + y a)) p
