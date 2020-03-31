{- |
Module      : Block6.Task3
Description : Simple parsers

Simple parsers (correct bracket sequence, integer).
-}
module Block6.Task3
  ( -- * Supporting types
    CBS(..)

    -- * Parsers
  , cbsParser
  , intParser
  ) where

import Control.Applicative (some, (<|>))
import Data.Char (digitToInt, isDigit)

import Block6.Task1 (Parser(..))
import Block6.Task2 (element, eof, maybeP, ok, satisfy)

-- | Data type representing a /correct bracket sequence/ in grammar
-- @cbs := <eps> | \'(\' cbs \')\' cbs@.
data CBS
  -- | Constructor of 'CBS' representing empty sequence.
  = Empty
  -- | Constructor of 'CBS' representing wrapping and concatenation.
  | WrapConcat CBS CBS
  deriving (Eq, Show)

-- | Object 'cbsParser' is an instance of parser from 'String' to 'CBS'.
-- Requires eof at the end of the sequence.
cbsParser :: Parser Char CBS
cbsParser = cbsParser' <* eof
  where
    cbsParser' :: Parser Char CBS
    cbsParser' =
      WrapConcat <$> left <*> right <|> Empty <$ ok
      where
        left, right :: Parser Char CBS
        left  = (element '(') *> cbsParser'
        right = (element ')') *> cbsParser'

-- | Object 'intParser' is an instance of parser from 'String' to 'Int'.
-- Parser any numeric before the integer and does not require eof after.
intParser :: Parser Char Int
intParser = (neg <|> pos) <*> digitsParser
  where
    neg, pos :: Parser Char (Int -> Int)
    neg = negate <$ element '-'
    pos = id <$ (maybeP $ element '+')
    digitsParser :: Parser Char Int
    digitsParser = fold <$> (some $ (fmap digitToInt $ satisfy isDigit))
      where
        fold :: Num a => [a] -> a
        fold = foldl1 $ \x y -> x * 10 + y
