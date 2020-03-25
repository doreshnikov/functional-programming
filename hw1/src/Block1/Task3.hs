{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Block1.Task3
Description : Algebraic data types - BST

Algebraic data type for BST and an interface for it.
-}
module Block1.Task3
  ( -- * Types
    Tree (..)

    -- * Functions
  , empty
  , find
  , fromList
  , insert
  , remove
  , size
  ) where

-- | Data type representing a binary tree node.
data Tree a
  -- | Constructor of 'Tree' representing empty leaf node.
  = Leaf
  -- | Record constructor of 'Tree' representing inner node.
  | Branch { -- | Field storing node's data.
             values :: [a]
             -- | Field storing left child.
           , left   :: Tree a
             -- | Field storing right child.
           , right  :: Tree a
           }
  deriving Show

-- | Function 'empty' checks whether 'Tree' of any type is empty or not.
empty :: Tree any -> Bool
empty Leaf = True
empty _    = False

-- | Function 'size' calculates a number of elements in a 'Tree' of any type.
size :: Tree any -> Int
size Leaf = 0
size Branch{..} = (length values) + (size left) + (size right)

-- | Function 'find' takes a /binary search tree/ and locates
-- a comparable value in it.
find :: (Ord a) => Tree a -> a -> Maybe a
find Leaf _ = Nothing
find Branch{..} (x :: a)
  | x == value = Just value
  | x < value  = find left x
  | otherwise  = find right x
    where
      value :: a
      value = head values

-- | Function 'insert' takes a /binary search tree/ and a comparable value
-- and returns a new tree with given value inserted into it.
insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Branch [x] Leaf Leaf
insert Branch{..} (x :: a)
  | x == value = Branch (x:values) left right
  | x < value  = Branch values (insert left x) right
  | otherwise  = Branch values left (insert right x)
    where
      value :: a
      value = head values

-- | Function 'fromList' takes a list of comparable values and creates
-- a new /binary search tree/ with all given values.
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insert Leaf

-- | Function 'remove' takes a /binary search tree/ and a comparable value
-- and returns a new tree with given value removed from it.
remove :: (Ord a) => Tree a -> a -> Tree a
remove Leaf _ = Leaf
remove Branch{..} (x :: a)
  | x == value = if length values > 1
                 then Branch (tail values) left right
                 else merge left right
  | x < value  = Branch values (remove left x) right
  | otherwise  = Branch values left (remove right x)
    where
      value :: a
      value = head values
      merge :: Tree a -> Tree a -> Tree a
      merge Leaf r              = r
      merge l Leaf              = l
      merge l (Branch v Leaf r) = Branch v l r
      merge l r                 = Branch (fst collect) l (snd collect)
        where
          collect :: ([a], Tree a)
          collect = extract r
            where
              extract :: Tree a -> ([a], Tree a)
              extract (Branch v Leaf r1) = (v, r1)
              extract (Branch v l1 r1)   = (fst next, Branch v (snd next) r1)
                where
                  next :: ([a], Tree a)
                  next = extract l1
              extract _                  = error "Invalid state"