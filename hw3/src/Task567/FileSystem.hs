{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Task567.FileSystem
-- Description : FileSystem data type and primitive lenses
--
-- FileSystem data type, primitive lenses, traversals, prisms and operators.
module Task567.FileSystem
  ( -- * Data types
    FS (..)

    -- * Functions
  , filteredChildren
  , readEntity
  , readDirectory
  , name
  , contents
  , contents'
  , _Dir
  , _File
  ) where

import Lens.Micro
import System.Directory (doesDirectoryExist, listDirectory)

-- | Data type representing file system tree with directories and files.
data FS
  -- | Constructor of a /directory/ entity in file system
  = Dir
    { -- | Directory name
      _name :: FilePath,
      -- | Directory contents
      _contents :: [FS]
    }
  -- | Constructor of a /file/ entity of file
  | File
    { -- | File name
      _name :: FilePath
    }
  deriving (Eq, Show)

-- | Reads an entity from a file system in 'IO' action.
readEntity :: FilePath -> IO FS
readEntity path = do
  isDir <- doesDirectoryExist path
  if isDir
    then readDirectory path
    else return $ File path

-- | Reads a directory ('Dir') entity from a file system in 'IO' action.
readDirectory :: FilePath -> IO FS
readDirectory path = Dir path <$> readContents path
  where
    readContents :: FilePath -> IO [FS]
    readContents p = do
      children <- listDirectory p
      mapM readEntity children

-- | A 'Lens'' from a 'FS' object into it's '_name' field.
name :: Lens' FS FilePath
name = lens _name $ \s newName -> s {_name = newName}

-- | A 'Lens'' from a 'FS' object into it' '_contents' field.
contents :: Lens' FS [FS]
contents = lens _contents $ \s newContents -> case s of
  Dir {..} -> s {_contents = newContents}
  file     -> file

-- | A prism-like 'Traversal'' from a 'FS' object into it's 'contents' lens.
contents' :: Traversal' FS [FS]
contents' f Dir {..} = Dir _name <$> f _contents
contents' _ file = pure file

-- | A prism ('Traversal'') from a 'FS' object into itself iff it's a 'Dir'.
_Dir :: Traversal' FS FS
_Dir f dir@Dir {..} = f dir
_Dir _ file = pure file

-- | A prism ('Traversal'') from a 'FS' object into itself iff it's a 'File'.
_File :: Traversal' FS FS
_File f file@File {..} = f file
_File _ dir = pure dir

-- | Helper 'Traversal'' builder equal to 'filtered' on the entity's '_contents'.
filteredChildren :: (FS -> Bool) -> Traversal' FS FS
filteredChildren p = contents' . each . filtered p
