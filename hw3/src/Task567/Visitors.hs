{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Task567.Visitors
-- Description : FileSystem advanced lenses and operations
--
-- Advanced complex combinations of 'FS' lenses and traversals and their
-- practical applications.
module Task567.Visitors
  ( -- * Lenses and traversals
    cd
  , extension
  , file
  , listRecursive
  , ls
  , mapAt
  , mapView
  , move

    -- * Operations based on lenses
  , file'
  , listDirRecursive
  , ls'
  , removeEmptyDir
  , replaceExtensionsAt
  ) where

import qualified Data.Map.Strict as M

import Lens.Micro
import System.FilePath (replaceExtension, takeExtension, joinPath)

import Task567.FileSystem

-- | Creates a prism-like 'Traversal'' from a directory to it's subdirectory.
cd :: FilePath -> Traversal' FS FS
cd path = filteredChildren (\x -> (_name x == path) && has _Dir x)

-- | A 'Traversal'' that lists all the directory's contents (names only).
ls :: Traversal' FS FilePath
ls = filteredChildren (const True) . name

-- | Returns all the directory's contents using 'ls'.
ls' :: FS -> [FilePath]
ls' item = item ^.. ls

-- | Creates a prism-like 'Traversal'' from a directory to a file in it.
file :: FilePath -> Traversal' FS FS
file path = filteredChildren (\x -> (_name x == path) && has _File x)

-- | Returns 'Just' a file's '_name' if it's in the directory
--  and 'Nothing' otherwise using 'file'.
file' :: FS -> FilePath -> Maybe FilePath
file' item path = item ^.. file path . name ^? each

-- | A 'Lens'' from a 'FS' object into it's extension.
extension :: Lens' FS String
extension = lens (takeExtension . _name) (\s newExt -> s {_name = replaceExtension (_name s) newExt})

-- | Replaces all the files' extensions in given directory using 'extension'.
replaceExtensionsAt :: FS -> FilePath -> FS
replaceExtensionsAt item newExt = item & filteredChildren (has _File) . extension .~ newExt

-- | A 'Traversal''-like object giving a view on /all/ contents in a
-- directory and all it's subdirectories.
listRecursive :: (Semigroup (f FS), Applicative f) => (FS -> f FS) -> FS -> f FS
listRecursive = lens id (const id) <> contents' . each . listRecursive

-- | Lists names of all the contents in a directory and it's subdirectories
-- using 'listRecursive'.
listDirRecursive :: FS -> [FilePath]
listDirRecursive item = item ^.. _Dir . listRecursive . name

-- | A 'Lens'' from an array of 'FS' to a 'M.Map' from each item's '_name'
-- to itself.
mapView :: Lens' [FS] (M.Map FilePath FS)
mapView = lens (\s -> M.fromList $ zip (map _name s) s) (\_ newMap -> map snd $ M.toList newMap)

-- | Creates a 'Lens'' from a 'M.Map' to 'Maybe' it's value by given key.
-- Allows an alteration and deletion.
mapAt :: FilePath -> Lens' (M.Map FilePath FS) (Maybe FS)
mapAt path = lens (M.lookup path) (\s newItem -> case newItem of
  Just item -> M.insert path item s
  Nothing   -> M.delete path s
  )

-- | Removes a subdirectory from given directory only in case it's empty
-- using 'mapView' and 'mapAt'.
removeEmptyDir :: FS -> FilePath -> FS
removeEmptyDir item path = item & contents . mapView . mapAt path . filtered emptyDir .~ Nothing
  where
    emptyDir :: Maybe FS -> Bool
    emptyDir = maybe False (\x -> has _Dir x && null (_contents x))

-- | /Advanced/ task. Creates a prism-like 'Traversal'' to an entity in
-- directory's '_contents' propagating full root-relative names.
--
-- Allows '(^.)' via 'name' but doesn't handle 'over' and 'set' in a good way.
move :: FilePath -> Traversal' FS FS
move path f Dir {_name = root, _contents = c} = Dir root <$> traverse (rename f) c
  where
    rename :: Applicative f => (FS -> f FS) -> FS -> f FS
    rename g item
      | path == _name item = g $ item {_name = joinPath [root, path]}
      | otherwise          = pure item
move _ _ fil = pure fil
