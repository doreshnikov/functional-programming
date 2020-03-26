{-# LANGUAGE TupleSections #-}

{- |
Module      : Block6.Task1
Description : Parser combinators - Primitives

Primitive parser combinators.
-}
module Block6.Task2
  ( -- * Types
    Parser(..)

  , element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Data.Function (fix)

import Block6.Task1

-- | Combinator 'ok': never fails, never consumes.
ok :: a -> Parser s a
ok x = Parser $ Just . (x, )

-- | Combinator 'eof': fails only on non-empty input.
eof :: Parser s Bool
eof = Parser $ \dat -> case dat of
  []   -> Just (True, [])
  _:_  -> Nothing

-- | Combinator 'satisfy': consumes all elements until the first one
-- satisfying given predicate then returns it.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ fix $ \f dat -> case dat of
  []   -> Nothing
  x:xs -> if p x then Just (x, xs) else f xs

-- | Combinator 'element': consumes and returns the first element.
element :: Parser s s
element = satisfy $ \_ -> True

-- | Combinator 'stream': consumes and returns a given amount
-- of first elements.
stream :: Int -> Parser s [s]
stream n | n < 0 = error "Non-negative stream size expected"
stream n = Parser $ \dat -> if length dat < n
                            then Nothing
                            else Just (take n dat, drop n dat)
