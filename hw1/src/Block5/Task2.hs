{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Block5.Task2
Description : 'State' Monad - /simple moving average/

Implementation of /simple moving average/ with 'State' Monad.
-}
module Block5.Task2
  ( moving
  ) where

import Control.Monad (forM)
import Control.Monad.State (evalState, get, put)

-- | Function 'moving' takes a natural number and a list of 'Fractional's
-- and returns a list of average values computed on slices of given size.
moving :: (Fractional a) => Integer -> [a] -> [a]
moving n _ | n <= 0 = error "Positive slice size expected"
moving n xs = evalState (forM xs next) []
  where
    next = \x -> do
      slice <- get
      put $ x : (take (fromIntegral n - 1) slice)
      updSlice <- get
      return $ (sum updSlice) / (fromIntegral $ length updSlice)