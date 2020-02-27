{- |
Module      : Task3
Description : Task3 functions

Definitions of some functions via /SK basis/.
-}
module Task3
  ( -- * Combinators
    k
  , s

    -- * Functions
  , identity
  , composition
  , contraction
  , permutation
  ) where

-- | Function 's' is the /S-combinator/.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Function 'k' is the /K-combinator/. It is the same as the standard 'const'.
k :: a -> b -> a
k = const

-- | Function 'composition' is the same as the standard '($)'.
--
-- Defined as @s (k s) k@.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

-- | Function 'identity' is the same as the standard 'id'.
--
-- Defined as @s k k@.
identity :: a -> a
identity = s k k

-- | Function 'contraction' passes it's second argument
-- to it's first argument twice.
--
-- Defined as @s s (s k)@.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s k)

-- | Function 'permutation' is the same as the standard 'flip'.
--
-- Defined as @s (s (k (s (k s) k)) s) (k k)@.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (k (s (k s) k)) s) (k k)
