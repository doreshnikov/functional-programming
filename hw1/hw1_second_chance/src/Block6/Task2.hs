{-# LANGUAGE TupleSections #-}

{- |
Module      : Block6.Task2
Description : Parser combinators - primitives

Primitive parser combinators.
-}
module Block6.Task2
  ( -- * Types
    Parser(..)

    -- * Combinators
  , anyP
  , element
  , eof
  , maybeP
  , ok
  , repeatP
  , satisfy
  , stream
  ) where

import Control.Applicative (empty, (<|>))

import Block6.Task1

-- | Combinator 'ok' never fails, never consumes.
ok :: Parser s ()
ok = pure ()

-- | Combinator 'eof' fails iff input is not empty.
eof :: Parser s ()
eof = Parser $ \dat -> case dat of
  [] -> Just ((), [])
  _  -> Nothing

-- | Combinator 'satisfy' consumes and returns the first element
-- iff it satisfies a given predicate, otherwise fails.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \dat -> case dat of
  []   -> Nothing
  x:xs -> if p x then Just (x, xs) else Nothing

-- | Combinator 'element' consumes and returns the first element iff
-- it is equal to a given one, otherwise fails.
element :: Eq s => s -> Parser s s
element = satisfy . (==)

-- | Combinator 'stream' consumes and returns a list of first elements
-- iff they are sequentially equal to a given list of elements.
stream :: Eq s => [s] -> Parser s [s]
stream []     = pure []
-- stream (x:xs) = (element x) >>= (\t -> fmap (t:) $ stream xs)
stream (x:xs) = (:) <$> element x <*> stream xs

-- | Combinator 'anyP' wraps a list of parsers and returns the first
-- successful parsing result and fails if none are successful.
anyP :: Eq s => [Parser s a] -> Parser s a
anyP = foldr1 (<|>)

-- | Combinator 'maybeP' wraps another parser and returns 'Just' it's value
-- on success and 'Nothing' on failure (but doesn't fail itself).
maybeP :: Parser s a -> Parser s (Maybe a)
maybeP p = (Just <$> p) <|> (Nothing <$ ok)

-- | Combinator 'repeatP' takes a number and a parser repeats a given parser
-- sequentially given number of times.
repeatP :: Int -> Parser s a -> Parser s [a]
repeatP n p
  | n < 0     = empty
  | n == 0    = pure []
  | otherwise = (:) <$> p <*> (repeatP (n - 1) p)