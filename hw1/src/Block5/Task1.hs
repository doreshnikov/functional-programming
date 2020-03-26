{- |
Module      : Block5.Task1
Description : Monadic calculations - arithmetic expressions

Data type 'Expr' for arithmetic expressions and calculation functions.
-}
module Block5.Task1
  ( -- * Types
    Expr(..)
  , ArithmeticError(..)

    -- * Functions
  , eval
  , evalBinary
  ) where

import Control.Applicative (liftA2)

-- | Data type 'Expr' represents a node in expression's parse tree.
data Expr
  -- | Constructor of 'Expr' representing an integer constant.
  = Const Int
  -- | Constructor of 'Expr' representing addition of two expressions.
  | Add Expr Expr
  -- | Constructor of 'Expr' representing subtraction of two expressions.
  | Sub Expr Expr
  -- | Constructor of 'Expr' representing multiplication of two expressions.
  | Mul Expr Expr
  -- | Constructor of 'Expr' representing integer division of two expressions.
  | Div Expr Expr
  -- | Constructor of 'Expr' representing exponentiation of two expressions.
  | Pow Expr Expr
  deriving Show

-- | Data type 'ArithmeticError' is used in event when
-- expression can't be evaluated correctly.
data ArithmeticError = ArithmeticError String deriving Show

-- | Function 'evalBinary' evaluates an expression corresponding to safe binary
-- operator. Output is either 'Left' 'ArithmeticError' or 'Right' @value@.
evalBinary :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
evalBinary op l r = liftA2 op (eval l) (eval r)
{-
evalBinary op l r = do
  left  <- eval l
  right <- eval r
  return (op left right)
-}

-- | Function 'eval' evaluates an expression using monadic 'Either'.
-- Output is either 'Left' 'ArithmeticError' or 'Right' @value@.
eval :: Expr -> Either ArithmeticError Int
eval (Const c) = Right c
eval (Add l r) = evalBinary (+) l r
eval (Sub l r) = evalBinary (-) l r
eval (Mul l r) = evalBinary (*) l r
eval (Div l r) = eval r >>= \x -> if x == 0
                                  then Left $ ArithmeticError "Division by zero"
                                  else eval l >>= return . (div x)
eval (Pow l r) = eval r >>= \x -> if x < 0
                                  then Left $ ArithmeticError "Negative exponent"
                                  else eval l >>= return . (^ x)
