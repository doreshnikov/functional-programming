{-# LANGUAGE LambdaCase #-}

{- |
Module      : Block1.Task1
Description : Algebraic data types - Weekdays

Algebraic data type for Weekdays and primitive interface for it.
-}
module Block1.Task1
  ( -- * Types
    Weekday (..)

    -- * Functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

-- | Type 'Weekday' represents one of the days of the week
data Weekday
  = Monday    -- ^  'Weekday' representing monday
  | Tuesday   -- ^  'Weekday' representing tuesday
  | Wednesday -- ^  'Weekday' representing wednesday
  | Thursday  -- ^  'Weekday' representing thursday
  | Friday    -- ^  'Weekday' representing friday
  | Saturday  -- ^  'Weekday' representing saturday
  | Sunday    -- ^  'Weekday' representing sunday
  deriving Show

-- | Function 'nextDay' takes a 'Weekday' object and returns the next day.
nextDay :: Weekday -> Weekday
nextDay = \case
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Function 'afterDays' takes a 'Weekday' and a 'Natural' number
-- and returns a day that happens this number of days after a given one.
afterDays :: Weekday -> Natural -> Weekday
afterDays day 0 = day
afterDays day n = nextDay $ afterDays day (n - 1)

-- | Function 'isWeekend' takes a 'Weekday' and returns
-- whether is's @Saturday@ or @Sunday@.
isWeekend :: Weekday -> Bool
isWeekend = \case
  Saturday  -> True
  Sunday    -> True
  _         -> False

-- | Function 'daysToParty' takes a 'Weekday' and returns
-- the number of days until the nearest @Friday@.
daysToParty :: Weekday -> Natural
daysToParty Friday = 0
daysToParty day = (+ 1) $ daysToParty $ nextDay day