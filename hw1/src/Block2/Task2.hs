{- |
Module      : Block2.Task2
Description : Folding for 'splitOn' and 'joinWith'

Functions for joining and splitting strings with folding.
-}
module Block2.Task2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- | Function 'splitOn' takes a 'Char' separator and a 'String' and returns
-- a 'NonEmpty' of it's substrings separated by given character.
splitOn :: Char -> String -> NonEmpty String
splitOn sep = foldr update ("" :| [])
  where
    update :: Char -> NonEmpty String -> NonEmpty String
    update c ne@(x :| xs)
      | c == sep  = "" <| ne
      | otherwise = ((c : x) :| xs)

-- | Function 'joinWith' takes a 'Char' separator and a 'NonEmpty' of 'String's
-- and returns a 'String' consisting of given substrings separated
-- by given character.
joinWith :: Char -> NonEmpty String -> String
joinWith sep = foldr1 (\s w -> s ++ sep : w)