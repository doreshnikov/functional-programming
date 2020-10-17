{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Task8.Grid
-- Description : Basic ListZipper and Grid and their primitives
--
-- Grid and ListZipper data types and primitive operations for them.
module Task8.Grid
  ( -- * Types
    ListZipper (..)
  , Grid (..)

    -- * Functions
  , constant
  , cutGrid
  , cutZipper
  , down
  , genericMove
  , horizontal
  , left
  , moveLeft
  , moveRight
  , neighbours
  , replace
  , replaceCenter
  , right
  , up
  , vertical
  ) where

import Control.Comonad

-- | Data type representing an infinite list with a selected middle element.
data ListZipper a = LZ [a] a [a]

-- | Constructs a 'ListZipper' with infinite copies of the same element.
constant :: a -> ListZipper a
constant x = LZ (repeat x) x (repeat x)

-- | Moves a 'ListZipper' one element to the left.
moveLeft :: ListZipper a -> ListZipper a
moveLeft (LZ (a:ls) x rs) = LZ ls a (x:rs)
moveLeft _ = error "right border"

-- | Moves a 'ListZipper' one element to the right.
moveRight :: ListZipper a -> ListZipper a
moveRight (LZ ls x (b:rs)) = LZ (x:ls) b rs
moveRight _ = error "left border"

-- | Wraps an element into 'ListZipper' iterating to the left and to the
-- right with given functions.
genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g x = LZ (tail $ iterate f x) x (tail $ iterate g x)

-- | Replaces a selected element of a 'ListZipper' with a given one.
replaceCenter :: ListZipper a -> a -> ListZipper a
replaceCenter (LZ ls _ rs) y = LZ ls y rs

-- | Cuts a 'ListZipper' leaving only a specified amount of elements
-- on both sides.
cutZipper :: Int -> ListZipper a -> [a]
cutZipper size (LZ ls x rs) = reverse (take size ls) ++ ([x] ++ take size rs)

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x
  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove moveLeft moveRight

-- | Data type representing an infinite grid with a selected middle element.
newtype Grid a = Grid
  { -- | A wrapper allowing a 'Grid' to be a 'Comonad' on it's own.
    unGrid :: ListZipper (ListZipper a)
  }

-- | Moves a 'Grid' one element up.
up :: Grid a -> Grid a
up (Grid grid) = Grid $ moveLeft grid

-- | Moves a 'Grid' one element down.
down :: Grid a -> Grid a
down (Grid grid) = Grid $ moveRight grid

-- | Moves a 'Grid' one element left.
left :: Grid a -> Grid a
left (Grid grid) = Grid $ fmap moveLeft grid

-- | Moves a 'Grid' one element right.
right :: Grid a -> Grid a
right (Grid grid) = Grid $ fmap moveRight grid

-- | An array of relevant 'Grid' direction modifications.
neighbours :: [Grid a -> Grid a]
neighbours = [up, right, down, left]

-- | Creates a 'ListZipper' of horizontally moved 'Grid's.
horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

-- | Creates a 'ListZipper' of vertically moved 'Grid's.
vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

-- | Replaces a selected element of a 'Grid' with a given one.
replace :: Grid a -> a -> Grid a
replace (Grid grid) x = Grid $ replaceCenter grid $ replaceCenter (extract grid) x

-- | Cuts a 'ListZipper' leaving only a specified amount of elements
-- in every direction.
cutGrid :: Int -> Grid a -> [[a]]
cutGrid size (Grid grid) = cutZipper size $ cutZipper size <$> grid

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid grid) = Grid $ fmap f <$> grid

instance Comonad Grid where
  extract :: Grid a -> a
  extract (Grid grid) = extract $ extract grid
  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical
