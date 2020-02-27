{- |
Module      : Task5
Description : Task5 functions

Basic operations for Church's numerals.
-}
module Task5
  ( -- * Types
    Nat

    -- * Functions
  , churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

-- | Type 'Nat' is a type of Church's numerals as the functions
-- which apply their first argument to the second multiple times.
type Nat a = (a -> a) -> a -> a

-- | Function 'zero' denotes @0@ in Church's numerals
-- which means it applies it's first argument to the second zero times
-- and returns the second argument itself.
zero :: Nat a
zero _ x = x

-- | Function 'succChurch' is the equivalent of @('+' 1)@ for Church's numerals
-- which means it creates a function which applies it's first argument to
-- the second exactly one time more than given Church's numeral.
succChurch :: Nat a -> Nat a
succChurch n f x = n f (f x)


-- | Function 'churchPlus' is the equivalent of '(+)' for Church's numerals
-- which means it creates a function which applies it's first argument to
-- the second the amount of times two given Church's numerals do in total.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)


-- | Function 'churchMult' is the equivalent of '(*)' for Church's numerals
-- which means it creates a function which applies it's first argument to
-- the second one the amount of times equal to the product of the application
-- times of two given Church's numerals.
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f = a (b f)

-- | Function 'churchToInt' transforms given Church's numeral into
-- corresponding 'Integer'.
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
