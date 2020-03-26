{- |
Module      : Block3.Task1
Description : Monoids - 'Maybe' and 'Either' edition

Functions 'maybeConcat' and 'eitherConcat' for wrapped monoids concatenation.
-}
module Block3.Task1
  ( eitherConcat
  , maybeConcat
  ) where

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

-- | Function 'maybeConcat' takes a list of 'Maybe's of lists
-- and concatenates all the lists wrapped inside.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = fromMaybe [] . mconcat

-- | Function 'eitherConcat' takes a 'Foldable' of 'Either's with any 'Monoid's
-- wrapped inside and concatenates them into pair of 'Monoid's of same types.
eitherConcat :: (Monoid m, Monoid n) => [Either m n] -> (m, n)
eitherConcat = bimap mconcat mconcat . partitionEithers
