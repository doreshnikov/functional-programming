{-# LANGUAGE TypeOperators #-}

{- |
Module      : Task1
Description : Task1 functions

Definitions of basic functions working with 'Either'.
-}
module Task1
  ( -- * Type operators
    type (<->)

    -- * Functions
  , associator
  , distributivity
  , eitherAssoc
  ) where

-- | The 'distributivity' function expands 'Either'
-- with the pair in the right side into pair of 'Either'.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity = either (\x -> (Left x, Left x)) (\x -> (Right $ fst x, Right $ snd x))

-- | The 'associator' function changes the associativity order
-- in twice nested pairs from right to left.
associator :: (a, (b, c)) -> ((a, b), c)
associator = applicator (applicator fst (fst . snd)) (snd . snd)
  where
    -- | The 'applicator' function takes two functions on pairs and a pair and
    -- returns the pair of those functions' return values calculated on given pair.
    applicator :: ((a, b) -> c) -> ((a, b) -> d) -> (a, b) -> (c, d)
    applicator f_fst f_snd t = (f_fst t, f_snd t)

-- | The '(<->)' TypeOperator denotes the pair of types
-- of transformer functions between two classes.
type (<->) a b = (a -> b, b -> a)

-- | The 'eitherAssoc' is a pair of of functions converting associativity order
-- in twice nested 'Either' between right and left.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (toLeftNesting, toRightNesting)
  where
    toLeftNesting  :: Either a (Either b c) -> Either (Either a b) c
    toLeftNesting  = either (Left . Left) (either (Left . Right) Right)
    toRightNesting :: Either (Either a b) c -> Either a (Either b c)
    toRightNesting = either (either Left (Right . Left)) (Right . Right)
