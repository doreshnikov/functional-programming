{- |
Module      : Task2
Description : Task2 functions

Definitions of functions inhabiting specific types corresponding
to some useful expressions in intuitionistic logic.
-}
module Task2
  ( -- * Types
    Neg

    -- * Functions
  , doubleNeg
  , doubleNegElim
  , excludedNeg
  , peirce
  , thirdNegElim
  ) where

import Data.Void (Void)

-- | Type 'Neg' is the type of a single-argument function returning 'Void'.
--
-- In intuitionistic logic it corresponds to @!a@ as a
-- way of constructing 'Void' from other value in BHK interpretation.
type Neg a = a -> Void

-- | Function 'doubleNeg' inhabits the type @a -> (a -> Void) -> Void@.
--
-- In intuitionistic logic it's type corresponds to @a -> !!a@
-- and in BHK interpretation it constructs 'Void' from an object of type @a@
-- and a method that constructs 'Void' from an object of type @a@.
doubleNeg :: a -> Neg (Neg a)
doubleNeg = flip ($)  -- \item -> \itemToVoid -> itemToVoid item

-- | Function 'excludedNeg' inhabits the type
-- @((Either a (a -> Void)) -> Void) -> Void@.
--
-- In intuitionistic logic it's type corresponds to @!!(a | !a)@
-- and in BHK interpretation it constructs 'Void' from a method that
-- constructs void from either @a@ or a method constructing 'Void' from @a@.
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg eitherToVoid = eitherToVoid (Right (buildAToVoidFrom eitherToVoid))
  where
    -- | Function 'buildAToVoidFrom' creates a function of type @a -> Void@
    -- from given 'eitherToVoid' passed from the higher level.
    buildAToVoidFrom :: (Either a (Neg a) -> Void) -> (a -> Void)
    buildAToVoidFrom = flip (.) Left  -- \eitherToVoid -> eitherToVoid . Left

-- | Function 'peirce' should inhabit the type @((a -> b) -> a) -> a)@.
--
-- In intuitionistic logic it's unprovable by not being a tautology
-- in a three-valued interpretation of intuitionistic logic.
-- <https://en.wikipedia.org/wiki/Three-valued_logic#Kleene_and_Priest_logics>
peirce :: ((a -> b) -> a) -> a
peirce = undefined

-- | Function 'doubleNegElim' should inhabit the type
-- @((a -> Void) -> Void) -> a@.
--
-- In intuitionistic logic it's type corresponds to @!!a -> a@ which is
-- unprovable since @'doubleNegElim' $ 'excludedNeg'@ has the type
-- corresponding to /the law of excluded middle/ which is equivalent to
-- unprovable /Peirce's law/ as shown in 'peirce'.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Function 'thirdNegElim' inhabits the type
-- @(((a -> Void) -> Void) -> Void) -> a -> Void@.
--
-- In intuitionistic logic it's type corresponds to @!!!a -> !a@
-- and in BHK interpretation it constructs a negation of @a@ from
-- triple negation of @a@.
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim tripleNeg a = tripleNeg ($ a)
-- ($ a) states for ((a -> Void) -> Void)
