{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Parser.Parser
Description : Parser combinators - instantiations

Instantiation of useful classes with data type 'Parser'.
-}
module Parser.Parser
  ( Parser(..)
  ) where

import Control.Applicative (Alternative, (<|>), empty)

import FSError

-- | Type 'Parser' represents a parser of given stream of objects
-- into 'Either' error or pair of resulting value and remaining objects.
newtype Parser s a =
  Parser
    { runParser :: [s] -> Either FSError (a, [s])
    }

-- | 'Parser' with fixed type argument is an instance of 'Functor'.
-- Values of this type can be mapped without unwrapping with 'fmap'.
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser (fmap (\(a, x) -> (f a, x)) . p)

-- | 'Parser' with fixed type argument is an instance of 'Applicative'.
-- Values of this type can be applied to each other re-wrapping the result
-- of inner application. The resulting 'Parser' will result in application
-- of two sequentially parsed results.
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ Right . (x, )
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) fp vp =
    Parser $ \dat -> do
      (f, res) <- runParser fp dat
      (v, out) <- runParser vp res
      return (f v, out)

-- | 'Parser' with fixed type argument is an instance of 'Monad'.
-- Values of this type can be viewed as states and actions with those
-- states can be sequentially composed.
instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) p f =
    Parser $ \dat -> do
      (a, res) <- runParser p dat
      runParser (f a) res

-- | 'Parser' with fixed type argument is an instance of 'Alternative'.
-- Values of this type can be alternated. Alternative between two 'Parser's
-- will choose the first non-'Nothing' from two given options.
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const $ Left $ ParserError "fail"
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) p1 p2 =
    Parser $ \dat ->
      case runParser p1 dat of
        Left _ ->
          case runParser p2 dat of
            Left _ -> Left $ ParserError "failed alternative"
            Right (a, res) -> return (a, res)
        Right (a, res) -> return (a, res)