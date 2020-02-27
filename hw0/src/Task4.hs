{- |
Module      : Task4
Description : Task4 functions

Definitions of some recursive functions using /Y-combinator/.
-}
module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- | Function 'iterateElement' returns an infinite 'Data.List.List'
-- containing given value.
iterateElement :: a -> [a]
iterateElement a = fix (\x -> a : x)

-- | Function 'fibonacci' takes an 'Integer' @n@ and returns
-- the @n@-th Fibonacci number.
fibonacci :: Integer -> Integer
fibonacci = fix (\f i -> if i < 2 then i else f (i - 1) + f (i - 2))

-- | Function 'factorial' takes an 'Integer' @n@ and returns
-- the factorial of @n@.
factorial :: Integer -> Integer
factorial = fix (\f i -> if i < 2 then 1 else i * f (i - 1))

-- | Function 'mapFix' takes a function and a 'Data.List.List' and transforms
-- all of the lists's elements with this function.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\f g l -> if null l then [] else g (head l) : f g (tail l))
