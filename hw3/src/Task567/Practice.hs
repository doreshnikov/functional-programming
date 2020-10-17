-- |
-- Module      : Task567.Practice
-- Description : Practice on lenses and traversals
--
-- Complex lenses and traversals with practical application of 'FS'.
module Task567.Practice
  ( -- * Lenses combinations
    addSuffix
  , allFiles
  , firstDir
  , listDir
  , listFiltered
  , toRoot
  , validateDir
  , validateFile
  ) where

import Lens.Micro
import Task567.FileSystem

-- | Lists all the directory contents and '[]' if not a directory.
listDir :: FS -> [FS]
listDir item = item ^.. contents' ^. _head

-- | Returns 'Just' a '_name' iff an argument is a directory
-- and 'Nothing' otherwise.
validateDir :: FS -> Maybe FilePath
validateDir item = item ^? (_Dir . name)

-- | Returns a '_name' iff an argument is a file
-- and an empty 'FilePath' otherwise.
validateFile :: FS -> FilePath
validateFile item = item ^? (_File . name) ^. non ""

-- | Returns a 'FS' same as the given one with it's '_name' changed to "/".
toRoot :: FS -> FS
toRoot item = item & (_Dir . name) .~ "/"

-- | Modifies the 'FS' appending a suffix to it's '_name'.
addSuffix :: String -> FS -> FS
addSuffix suffix item = item & name %~ (++ suffix)

-- | Lists all the directory's contents filtered by predicate
-- and '[]' if not a directory.
listFiltered :: (FS -> Bool) -> FS -> [FS]
listFiltered p item = item ^? (_Dir . contents) ^. non [] ^.. each . filtered p

-- | Returns 'Just' a first 'Dir' entry in given directory
-- and 'Nothing' if can't be found.
firstDir :: FS -> Maybe FilePath
firstDir item = listFiltered (has _Dir) item ^.. each . name ^? _head

-- | Lists all the 'File' entities in given directory
-- and '[]' if not a directory.
allFiles :: FS -> [FilePath]
allFiles item = listFiltered (has _File) item ^.. each . name
