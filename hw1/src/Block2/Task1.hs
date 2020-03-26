{-# LANGUAGE InstanceSigs, RecordWildCards, ScopedTypeVariables #-}

{- |
Module      : Block2.Task1
Description : Foldable instantiation for Block1.Task3.Tree

Instantiation of 'Foldable' with a data type 'Tree' from Block1.Task3.
-}
module Block2.Task1
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
