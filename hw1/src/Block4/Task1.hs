{- |
Module      : Block4.Task1
Description : Functors - 'stringSum'

Function 'stringSum' adding up all the numbers in given 'String'.
-}
module Block4.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Function 'stringSum' performs a safe addition of all numbers
-- in given 'String'. In case of invalid input it returns 'Nothing'.
stringSum :: String -> Maybe Int
stringSum = (sum <$>) . traverse readMaybe . words
