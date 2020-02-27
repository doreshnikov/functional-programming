{- |
Module      : Task7
Description : Task7 functions

Subexpression types.
-}
module Task7
  ( a
  , b
  ) where

import Data.Either (lefts, rights)

-- | The first expression for analysis.
a :: Bool
-- a = null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
-- a = ($) (null . head) (map (uncurry id) [((++) "Dorian ", " Grey")])
a = applyNullHead mapResult
  where
    applyNullHead :: [[Char]] -> Bool
    -- applyNullHead = ($) (null . head)
    applyNullHead = apply nullHead
      where
        apply     :: (a -> b) -> a -> b
        apply     = ($)
        nullHead  :: Foldable t => [t a] -> Bool
        -- nullHead = null . head
        nullHead  = compositionMyNull myHead
          where
            compositionMyNull :: Foldable t => (x -> t a) -> x -> Bool
            compositionMyNull = composition myNull
              where
                composition :: (b -> c) -> (a -> b) -> a -> c
                composition = (.)
                myNull      :: Foldable t => t a -> Bool
                myNull      = null
            myHead            :: [a] -> a
            myHead            = head
    mapResult     :: [[Char]]
    -- mapResult = map (uncurry id) [((++) "Dorian ", " Grey")]
    mapResult     = myMapFunction list
      where
        myMapFunction :: [(a -> b, a)] -> [b]
        myMapFunction = myMap function
          where
            myMap    :: (a -> b) -> [a] -> [b]
            myMap    = map
            function :: (a -> b, a) -> b
            -- function = uncurry id
            function = myUncurry myId
              where
                myUncurry :: (a -> b -> c) -> (a, b) -> c
                myUncurry = uncurry
                myId      :: a -> a
                myId      = id
        list          :: [([Char] -> [Char], [Char])]
        -- list = [((++) "Dorian ", " Grey")]
        list          = one tuple
          where
            one   :: a -> [a]
            one x = [x]
            tuple :: ([Char] -> [Char], [Char])
            -- tuple = ((++) "Dorian ", " Grey")
            tuple = commaLeft right
              where
                commaLeft :: b -> ([Char] -> [Char], b)
                commaLeft = comma left
                  where
                    comma :: a -> b -> (a, b)
                    comma = (,)
                    left  :: [Char] -> [Char]
                    -- left = (++) "Dorian "
                    left  = myConcat dorian
                      where
                        myConcat :: [Char] -> [Char] -> [Char]
                        myConcat = (++)
                        dorian   :: [Char]
                        dorian   = "Dorian "
                right :: [Char]
                right = " Grey"

-- | The second expression for analysis.
b :: (Num x, Num y) => [(x, y)]
-- b = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
b = lambda list
  where
    lambda :: (Num a, Num b) => [Either a b] -> [(a, b)]
    -- lambda = \x -> zip (lefts x) (rights x)
    -- x :: [Either a b]
    {-
    Some of the next subexpressions' types are commented
    because they are only valid when fixing x's @a@ and @b@
    and it seems kind of impossible with my current knowledge
    -}
    lambda x = zipLeftsX rightsX
      where
--        zipLeftsX :: [b] -> [(a, b)]
        zipLeftsX = myZip leftsX
          where
            myZip  :: [a] -> [b] -> [(a, b)]
            myZip  = zip
--            leftsX :: [a]
            leftsX = myLefts x
              where
                myLefts :: [Either a b] -> [a]
                myLefts = lefts
--        rightsX   :: [b]
        rightsX   = myRights x
          where
            myRights :: [Either a b] -> [b]
            myRights = rights
    list :: (Num a, Num b) => [Either a b]
    -- list = [Left (1 + 2), Right (2 ^ 6)]
    list = twoFirst second
      where
        twoFirst :: (Num a, Num b) => Either a b -> [Either a b]
        twoFirst = build first
          where
            build   :: a -> a -> [a]
            -- build = \x -> \y -> `[](createList)` x y
            build x y = [x, y]
            first :: Num a => Either a x
            -- first = Left (1 + 2)
            first = myLeft three
              where
                myLeft :: a -> Either a x
                myLeft = Left
                three  :: Num a => a
                -- sum = 1 + 2
                three  = plusOne two
                  where
                    plusOne :: Num a => a -> a
                    -- plusOne = (1 +)
                    plusOne = myPlus one
                      where
                        myPlus :: Num a => a -> a -> a
                        myPlus = (+)
                        one    :: Num p => p
                        one    = 1
                    two     :: Num a => a
                    two     = 2
        second   :: (Num a, Num b) => Either a b
        -- second = (Right (2 ^ 6))
        second   = myRight value
          where
            myRight :: a -> Either x a
            myRight = Right
            value   :: Num a => a
            -- value = 2 ^ 6
            value   = powerTwo six
              where
                powerTwo :: (Integral b, Num a) => b -> a
                -- powerTwo = (2 ^)
                powerTwo = power two
                  where
                    power :: (Integral b, Num a) => a -> b -> a
                    power = (^)
                    two   :: Num a => a
                    two   = 2
                six      :: Integral b => b
                six      = 6
