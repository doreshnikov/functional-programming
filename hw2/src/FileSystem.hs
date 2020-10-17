{- |
Module      : FileSystem
Description : 'FileSystem' type and basic operations. 

Operations for reading and dumping 'FileSystem' via communication
with real world.
-}
module FileSystem
  ( -- * Data types
    Dir(..)
  , Entity(..)
  , File(..)
  , FileSystem(..)

    -- * Operations and utilities
  , dumpFileSystem
  , getName
  , pprint
  , readFileSystem
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M

import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  , removeDirectoryRecursive
  , setCurrentDirectory
  )
import System.FilePath ((</>))

-- | Data type 'File' represents file entity in file system.
data File = File
  { -- | Method that returns full absolute 'FilePath'.
    getFileName :: FilePath
    -- | Method that returns file contents as 'BS.ByteString'.
  , getData :: BS.ByteString
  } deriving (Show)

-- | Data type 'Dir' represents directory entity in file system.
data Dir = Dir
  { -- | Method that returns full absolute 'FilePath'.
    getDirName :: FilePath
    -- | Method that returns 'M.Map' of directory's contents.
  , getItems :: M.Map FilePath Entity
  } deriving (Show)

-- | Data type 'Entity' represents any entity in file system.
data Entity
    -- | 'F' constructor represents a 'File' entity.
  = F File
    -- | 'D' constructor represents a 'Dir' entity.
  | D Dir
    deriving (Show)

-- | Utility function wrapping 'getFileName' and 'getDirName'.
getName :: Entity -> FilePath
getName (F a) = getFileName a
getName (D b) = getDirName b

-- | Data type 'FileSystem' represents file system mirror
-- containing uppermost root and information about current directory.
data FileSystem = FileSystem
  { -- | Method that returns root 'Dir' object.
    getRoot :: Dir
    -- | Method that returns current directory 'Dir' object.
  , getLocation :: Dir
  } deriving (Show)

-- | Utility function. Reads file entity by given 'FilePath'
-- into 'IO'-wrapped 'File'.
readFileEntity :: FilePath -> IO File
readFileEntity path = do
  contents <- BS.readFile path
  return $ File path contents

-- | Utility function. Reads directory entity by given 'FilePath'
-- into 'IO'-wrapped 'Dir'.
readDirEntity :: FilePath -> IO Dir
readDirEntity path = do
  children <- listDirectory path
  Dir path <$> collectEntities children
  where
    collectEntities :: [FilePath] -> IO (M.Map FilePath Entity)
    collectEntities = foldr (liftM2 (\a b -> M.insert (getName a) a b) . readEntity . (</>) path) mempty

-- | Utility function wrapping 'readFileEntity' and 'readDirEntity'.
readEntity :: FilePath -> IO Entity
readEntity path = do
  isFile <- doesFileExist path
  if isFile
    then F <$> readFileEntity path
    else D <$> readDirEntity path

-- | 'FileSystem' reader that reads file system with current directory as root.
readFileSystem :: IO FileSystem
readFileSystem = do
  root <- getCurrentDirectory
  rootEntity <- readDirEntity root
  return $ FileSystem rootEntity rootEntity

-- | 'FileSystem' writer that writes file system on specified root path.
dumpFileSystem :: FileSystem -> IO FileSystem
dumpFileSystem fs = do
  let root = getDirName $ getRoot fs
  removeDirectoryRecursive root
  createDirectoryIfMissing False root
  let dump entity =
        case entity of
          (F file) -> BS.writeFile (getFileName file) (getData file)
          (D dir) -> do
            createDirectoryIfMissing False (getDirName dir)
            mapM_ dump (getItems dir)
  dump $ D $ getRoot fs
  setCurrentDirectory $ getDirName $ getLocation fs
  return fs

-- | Pretty-printing function for 'FileSystem'
pprint :: FileSystem -> String
pprint = pprintD 0 . getRoot

-- | Pretty-printing function for 'Entity'.
pprintE :: Int -> Entity -> String
pprintE x (F a) = replicate x ' ' ++ replicate x ' ' ++ getFileName a
pprintE x (D b) = replicate x ' ' ++ replicate x ' ' ++ pprintD x b

-- | Pretty-printing function for 'Dir'.
pprintD :: Int -> Dir -> String
pprintD x d = getDirName d ++ foldr (\z y -> "\n" ++ pprintE (x + 1) z ++ y) "" (getItems d)