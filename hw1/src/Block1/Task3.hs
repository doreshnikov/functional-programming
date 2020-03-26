{-# LANGUAGE InstanceSigs, RecordWildCards, ScopedTypeVariables #-}

{- |
Module      : Block1.Task3
Description : Algebraic data types - BST

Algebraic data type for BST and an interface for it.
-}
module Block1.Task3
  ( -- * Types
    Tree(..)

    -- * Functions
  , empty
  , find
  , fromList
  , insert
  , remove
  , size
  ) where

import Interblock.Tree
