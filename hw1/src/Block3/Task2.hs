{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Block3.Task2
Description : Semigroups & Monoids - 'NonEmpty' and more fun stuff edition

Instantiations of 'Semigroup's and 'Monoid's with custom data types.
-}
module Block3.Task2
  ( -- * Types
    Endo(..)
  , Name(..)
  , NonEmpty(..)
  , ThisOrThat(..)

    -- * Functions
  , toList
  ) where

-- | Data type 'NonEmpty' is an alias for the type 'Data.List.NonEmpty'.
data NonEmpty a = a :| [a]
  deriving Show

-- | Function 'toList' converts 'NonEmpty' to 'Data.List'.
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

-- | 'NonEmpty' is an instance of 'Semigroup'.
-- Values of this type can be concatenated using '(<>)'.
instance Semigroup (NonEmpty a) where
  (<>) :: (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a)
  (<>) (x :| []) = (:|) x . toList
  (<>) (x :| xs) = (:|) x . flip (foldr (:)) xs . toList

-- | Data type 'ThisOrThat' represents /at least one of two options/.
data ThisOrThat a b
  -- | Constructor of 'ThisOrThat' by the first option.
  = This a
  -- | Constructor of 'ThisOrThat' by the second option.
  | That b
  -- | Constructor of 'ThisOrThat' by both options.
  | Both a b

-- | 'ThisOrThat' is an instance of 'Semigroup'.
-- Values of this type can be concatenated using '(<>)'.
-- The result contains the leftmost value of the first type
-- and the rightmost value of the second type.
instance Semigroup (ThisOrThat a b) where
  (<>) :: (ThisOrThat a b) -> (ThisOrThat a b) -> (ThisOrThat a b)
  (<>) (This x) (That y)     = Both x y
  (<>) (That y) (This x)     = Both x y
  (<>) (That _) r            = r
  (<>) l (This _)            = l
  (<>) (This x) (Both _ y)   = Both x y
  (<>) (Both x _) (That y)   = Both x y
  (<>) (Both x _) (Both _ y) = Both x y

-- | Type 'Name' is an alias for the type 'String'.
newtype Name = Name { -- | Field 'name' returns a wrapped 'String'.
                      name :: String
                    }

-- | 'Name' is an instance of 'Semigroup'.
-- Values of this type can be concatenated using '(<>)'.
-- Values are concatenated with dot separator.
instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) x y = Name $ (name x) ++ "." ++ (name y)

-- | 'Name' is an instance of 'Monoid'.
-- This type has a 'mempty' value equal to 'Name' with empty 'String'.
instance Monoid Name where
  mempty :: Name
  mempty = Name ""

-- | Type 'Endo' is an alias for a function with same type argument and value.
newtype Endo a = Endo { -- | Field 'getEndo' returns a wrapped function.
                        getEndo :: a -> a
                      }

-- | 'Endo' is an instance of 'Semigroup'.
-- Values of the same 'Endo' types can be concatenated using '(<>)'.
-- Concatenation returns a composition of wrapped functions.
instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) x y = Endo $ (getEndo x . getEndo y)

-- | 'Endo' is an instance of 'Monoid'.
-- This type has a 'mempty' value equal to 'id'.
instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
