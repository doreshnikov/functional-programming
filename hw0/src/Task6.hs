{- |
Module      : Task6
Description : Task6 functions

WHNFs for some example expressions.
-}
module Task6
  ( -- * Support
    foo

    -- * Expressions
  , a
  , aResult
  , b
  , bResult
  ) where

import Data.Maybe (mapMaybe)

import Task1 (distributivity)

a :: (Either String a, Either String b)
a = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
{-^
@a = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))@

* expanding the definition of 'distributivity'

@a = either
  (\x -> (Left x, Left x))
  (\x -> (Right $ fst x, Right $ snd x))
  (Left ("harold" ++ " hide " ++ "the " ++ "pain"))@

* reducing the application of 'either'

@a = (\x -> (Left x, Left x)) ("harold" ++ " hide " ++ "the " ++ "pain")@

* reducing beta-redex

@a = ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
    , Left ("harold" ++ " hide " ++ "the " ++ "pain"))@
-}

-- | Equals to @(Left ("harold" ++ " hide " ++ "the " ++ "pain"), \
-- Left ("harold" ++ " hide " ++ "the " ++ "pain"))@.
--
-- In WHNF due to 'Data.Tuple.(,)' being the constructor of
-- 'Data.Tuple.Tuple' with two arguments.
aResult :: (Either String a, Either String b)
aResult = ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
          , Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Function converting \'o\' to @'Just' ('exp' 'pi')@
-- and other characters to 'Nothing'.
foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True  -> Just $ exp pi
      False -> Nothing

-- not actually, but good enough for this task

{-
null :: [a] -> Bool
null [] = True
null _  = False

mapMaybe            :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []       = []
mapMaybe f (x : xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r : rs
-}

b :: Bool
b = null $ mapMaybe foo "pole chudes ochen' chudesno"
{-^
@b = null $ mapMaybe foo "pole chudes ochen' chudesno"@

* expanding the application of '($)'

@b = null (mapMaybe foo "pole chudes ochen' chudesno")@

* reducing an argument for pattern-matching in 'null' /until it reaches whnf/

* reducing an argument for pattern-matching in 'mapMaybe'

@b = null (mapMaybe foo (\'p\' : "ole chudes ochen' chudesno"))@

* expanding the application of 'mapMaybe'

@b = null (let rs = mapMaybe foo "ole chudes ochen' chudesno" in
  case foo \'p\' of
    Nothing -> rs
    Just r  -> r : rs)@

* expanding @let@, reducing an argument for case-matching in 'foo'

@b = null (mapMaybe foo "ole chudes ochen' chudesno")@

* reducing an argument for pattern-matching in 'mapMaybe'

@b = null (mapMaybe foo (\'o\' : "le chudes ochen' chudesno"))@

* expanding the application of 'mapMaybe'

@b = null (let rs = mapMaybe foo "le chudes ochen' chudesno" in
  case foo \'o\' of
    Nothing -> rs
    Just r  -> r : rs)@

* expanding @let@, reducing an argument for case-matching in 'foo'

@b = null (exp pi : mapMaybe foo "ole chudes ochen' chudesno")@

* reducing application of 'null'

@b = False@
-}

-- | Equals to @'False'@.
--
-- In WHNF due to 'False' being the instance of 'Bool' and having no redexes.
bResult :: Bool
bResult = False
