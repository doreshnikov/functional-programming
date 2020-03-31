{- |
Module      : Block6.Task4
Description : listListParser

Complex parser (list of list of ints, comma-separated).
-}
module Block6.Task4
 ( listListParser
 ) where

import Control.Applicative (many, (<|>))

import Block6.Task1 (Parser(..))
import Block6.Task2 (element, eof, ok, repeatP)
import Block6.Task3 (intParser)

listListParser :: Parser Char [[Int]]
listListParser
  = [] <$ spaces <* eof <|>
    spaces *> ((:) <$> listParser <*> (many $ wrapped listParser) <|> ([] <$ ok)) <* spaces <* eof
  where
    spaces :: Parser Char String
    spaces = many $ element ' '
    wrapped :: Parser Char a -> Parser Char a
    wrapped p = spaces *> (element ',') *> spaces *> p
    listParser :: Parser Char [Int]
    listParser = intParser >>= \size ->
      repeatP size $ wrapped intParser
