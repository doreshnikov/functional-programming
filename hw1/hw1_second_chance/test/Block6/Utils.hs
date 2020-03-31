module Block6.Utils
  ( extract
  , remains
  ) where

extract :: Maybe (a, [s]) -> a
extract (Just (x, _)) = x
extract _ = error "extract failed"

remains :: Maybe (a, [s]) -> [s]
remains (Just (_, xs)) = xs
remains _ = error "remains failed"