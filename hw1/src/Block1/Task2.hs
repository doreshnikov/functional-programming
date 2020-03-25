{-# LANGUAGE InstanceSigs #-}

{- |
Module      : Block1.Task2
Description : Algebraic data types - Nat

Algebraic data type for Nat and an interface for it.
-}
module Block1.Task2
  ( Nat (..)
  ) where

import Data.Function (fix)

-- | Type 'Nat' represents a non-negative integer in Peano's arithmetic.
data Nat
  -- | Constructor of 'Nat' representing zero value
  = Z
  -- | Constructor of 'Nat' representing the value following the given one
  | S Nat
  deriving Show

-- | 'Nat' is an instance of 'Eq'.
-- Values of this type can be checked for equality.
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z         = True
  (==) (S n) (S m) = (==) n m
  (==) _ _         = False

-- | 'Nat' is an instance of 'Ord'.
-- Values of this type can be compared with each other.
instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) Z _         = True
  (<=) (S n) (S m) = (<=) n m
  (<=) _ _         = False

-- | 'Nat' is an instance of 'Num'.
-- Values of this type can be added, subtracted and multiplied
-- as well as converted from an 'Integer'.
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) Z     = id
  (+) (S n) = S . (n +)

  (-) :: Nat -> Nat -> Nat
  (-) n Z         = n
  (-) Z _         = Z
  (-) (S n) (S m) = (-) n m

  (*) Z _     = Z
  (*) (S n) m = (+ m) $ (*) n m

  abs      :: Nat -> Nat
  abs      = id
  signum   :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

  fromInteger :: Integer -> Nat
  fromInteger x
    | x < 0     = error "Nat can not be negative"
    | x == 0    = Z
    | otherwise = S $ fromInteger (x - 1)

-- | 'Nat' is an instance of 'Bounded'.
-- Specifically there is a calculable lower bound for values of this type.
instance Bounded Nat where
  minBound :: Nat
  minBound = Z
  maxBound :: Nat
  maxBound = fix S

-- | 'Nat' is an instance of 'Enum'.
-- Values of this type can be enumerated and have a next or a previous value.
instance Enum Nat where
  succ :: Nat -> Nat
  succ = S
  pred :: Nat -> Nat
  pred Z     = error "Z has no pred"
  pred (S n) = n

  toEnum :: Int -> Nat
  toEnum = fromInteger . toInteger

  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S n) = (+ 1) $ fromEnum n

-- | 'Nat' is an instance of 'Real'.
-- This instantiation is required by 'Integral'.
instance Real Nat where
  toRational :: Nat -> Rational
  toRational = toRational . fromEnum

-- | 'Nat' is an instance of 'Integral'.
-- Values of this type can be converted to 'Integer'
-- and divided with remainder.
instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger Z     = 0
  toInteger (S n) = (+ 1) $ toInteger n

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem _ Z = error "Division by zero"
  quotRem n m
    | n < m     = (0, n)
    | otherwise = incFst $ quotRem (n - m) m
      where
        incFst :: (Enum a) => (a, a) -> (a, a)
        incFst (x, y) = (succ x, y)