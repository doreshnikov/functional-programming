{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Block4.Task3
Description : Functors - 'Block3.Task2.NotEmpty'

'NonEmpty' data type from 'Block3.Task2' and it's instantiations
for 'Traversable' and 'Monad'.
-}
module Block4.Task3
  ( -- * Types
    NonEmpty(..)

    -- * Functions
  , toList
  , (<+>)
  ) where

import Interblock.NonEmpty