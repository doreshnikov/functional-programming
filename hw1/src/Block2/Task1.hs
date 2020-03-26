{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE InstanceSigs, RecordWildCards, ScopedTypeVariables #-}

{- |
Module      : Block2.Task1
Description : Foldable instantiation for Block1.Task3.Tree

Instantiation of 'Foldable' with a data type 'Tree' from Block1.Task3.
-}
module Block2.Task1
  ( Tree(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty (map, toList)

import Block1.Task3 (Tree(..))

-- | 'Data.List.NonEmpty.toList' alias.
toList' :: NonEmpty a -> [a]
toList' = Data.List.NonEmpty.toList

-- | 'Data.List.NonEmpty.map' alias.
map' :: (a -> b) -> NonEmpty a -> NonEmpty b
map' = Data.List.NonEmpty.map

-- | 'Tree' is an instance of 'Foldable'.
-- Values of this type can be folded with 'foldMap' or 'foldr'
-- and all derived folds.
instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf       = mempty
  foldMap f Branch{..} = (foldMap f left) `mappend`
                         (mconcat . toList' $ map' f values) `mappend`
                         (foldMap f right)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ eps Leaf                   = eps
  foldr f (eps :: b) (Branch{..} :: Tree a) = (fold' right) .
                                              (fold' values) .
                                              (fold' left) $ eps
    where
      fold' :: Foldable t => t a -> b -> b
      fold' = flip (foldr f)
