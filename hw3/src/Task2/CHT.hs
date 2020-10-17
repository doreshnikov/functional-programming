-- |
-- Module      : Task2.CHT
-- Description : Concurrent Hash Table
--
-- Concurrent Hash Table data type and a primitive functions (nope).
module Task2.CHT
  ( ConcurrentHashTable(..)
  ) where

import Control.Concurrent.STM
import Data.Vector

-- | A data type representing concurrent hash map with specified
-- hash function.
data ConcurrentHashTable k v = ConcurrentHashTable
  { -- | Items stored in a hash table
    items :: TVar (Vector (k, v))
    -- | Size of a hash table
  , size :: TVar Int
    -- | Hash function for key comparison
  , hashFunc :: k -> Int
  }
