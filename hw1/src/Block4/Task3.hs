{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Block4.Task3
Description : Functors - 'Block3.Task2.NotEmpty'

'NonEmpty' data type from 'Block3.Task2' and it's instantiations
for 'Traversable' and 'Monad'.
-}
module Block4.Task3
  ( -- * Types
    NonEmpty (..)

    -- * Functions
  , toList
  , (<+>)
  ) where

import Block3.Task2 ( NonEmpty (..)
                    , toList)

-- | Function '(<+>)' joins a 'NonEmpty' and a 'Data.List' of the same type
-- into one 'NonEmpty'.
infixr 5 <+>
(<+>) :: NonEmpty a -> [a] -> NonEmpty a
(<+>) (x :| xs) = (x :|) . (xs ++)

-- | 'NonEmpty' is an instance of 'Functor'.
-- Values of this type can be mapped without unwrapping with 'fmap'.
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = (f x) :| (fmap f xs)

-- | 'NonEmpty' is an instance of 'Applicative'.
-- Values of this type can be applied to each other re-wrapping the result
-- of inner application. The resulting 'NonEmpty' will have length equal
-- to the shortest length of two original 'NonEmpty's
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = (:| [])
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| (fs <*> xs)

-- | 'NonEmpty' is an instance of 'Monad'.
-- Values of this type can be viewed as states and actions with those
-- states can be sequentially composed.
instance Monad NonEmpty where
  (>>=) :: (NonEmpty a) -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) f = (f x) <+> (xs >>= toList . f)

-- | 'NonEmpty' is an instance of 'Foldable'.
-- Values of this type can be folded with 'foldMap' or 'foldr'
-- and all derived folds.
instance Foldable NonEmpty where
  foldMap :: (Monoid m) => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = (f x) <> (foldMap f xs)

-- | 'NonEmpty' is an instance of 'Traversable'.
-- Values of this type can be traversed by any 'Applicative'.
instance Traversable NonEmpty where
  traverse :: (Applicative f) => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse t (x :| xs) = (:|) <$> (t x) <*> (traverse t xs)