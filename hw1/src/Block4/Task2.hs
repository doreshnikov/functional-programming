{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Block4.Task2
Description : Functors - 'Tree'

Primitive 'Tree' data type and it's instantiations for 'Traversable'.
-}
module Block4.Task2
  ( Tree (..)
  ) where

-- | Data type 'Tree' represents a binary tree node.
data Tree a
  -- | Constructor of 'Tree' representing empty leaf node.
  = Leaf a
  -- | Constructor of 'Tree' representing inner node.
  | Branch (Tree a) (Tree a)

-- | 'Tree' is an instance of 'Functor'.
-- Values of this type can be mapped without unwrapping with 'fmap'.
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- | 'Tree' is an instance of 'Applicative'.
-- Values of this type can be applied to each other re-wrapping the result
-- of inner application. The resulting 'Tree' will have the smallest
-- common supershape of two original 'Tree's.
instance Applicative Tree where
  pure :: a -> Tree a
  pure x = Leaf x
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) (Leaf x)         = Leaf (f x)
  (<*>) app@(Leaf _) (Branch l r) = Branch (app <*> l) (app <*> r)
  (<*>) (Branch l r) val@(Leaf _) = Branch (l <*> val) (r <*> val)
  (<*>) (Branch l r) (Branch x y) = Branch (l <*> x) (r <*> y)

-- | 'Tree' is an instance of 'Foldable'.
-- Values of this type can be folded with 'foldMap' or 'foldr'
-- and all derived folds.
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (Leaf x)     = f x
  foldMap f (Branch l r) = (foldMap f l) `mappend` (foldMap f r)

-- | 'Tree' is an instance of 'Traversable'.
-- Values of this type can be traversed by any 'Applicative'.
instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse t (Leaf x)     = Leaf <$> t x
  traverse t (Branch l r) = Branch <$> traverse t l <*> traverse t r