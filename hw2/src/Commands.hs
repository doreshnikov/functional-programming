{- |
Module      : Commands
Description : Commands and utilities for 'FileSystem'

Command line commands, 'Call' wrapper and 'Parser's.
-}
module Commands
  ( -- * Command type template and datatype
    Call(..)
  , Command

    -- * Utilities
  , callParser
  , cutLast
  , cutPath
  , move
  , normalize
  , pathParser
  , propagate
  , propagateOnce
  , relocate
  , relocateBack
  , relocateMany
  , relocateOnce
  , resolvePath
  , toRoot

    -- * Operations
  , cat
  , cd
  , ls
  , mkdir
  , touch
  ) where

import Control.Applicative ((<|>), many)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, put)
import Data.Functor (($>))

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (isPrefixOf)
import qualified Data.Map as M

import System.FilePath ((</>), isRelative, pathSeparator)

import FileSystem

import FSError
import Parser.Combinators
import Parser.Parser()

-- | Represents a command type working with 'FileSystem' 'StateT' and
-- having access to 'IO' that returns @a@ as a meta-information
-- used in following computations.
type Command a = ExceptT FSError (StateT FileSystem IO) a

-- | Data type 'Call' represents a console command type being either
-- 'Exit' or a 'Command' with @()@ (@void@) return value.
data Call
    -- 'Call' constructor represents a 'Command' with no return value.
  = Call (Command ())
    -- 'Exit' constructor represents a finishing command.
  | Exit

-- | Utility function. Creates a 'Command' replacing current directory
-- in 'FileSystem'.
move :: Dir -> Command FileSystem
move d = do
  x <- get
  let y = FileSystem (getRoot x) d
  put y
  return y

-- | Utility function. Normalizes a 'FilePath' containing dots and double-dots.
normalize :: FilePath -> FilePath
normalize path = checkHead (foldr1 (</>) $ reverse $ noDots 0 $ reverse (cutPath "" path))
  where
    checkHead :: FilePath -> FilePath
    checkHead paths =
      if head path == pathSeparator
        then pathSeparator : paths
        else paths
    noDots :: Int -> [FilePath] -> [FilePath]
    noDots _ [] = []
    noDots n ("":xs) = noDots n xs
    noDots n (".":xs) = noDots n xs
    noDots n ("..":xs) = noDots (n + 1) xs
    noDots 0 (x:xs) = x : noDots 0 xs
    noDots n (_:xs) = noDots (n - 1) xs

-- | Utility function. Creates a 'Command' that normalizes path relatively to
-- 'FileSystem' root and current directory.
resolvePath :: FilePath -> Command FilePath
resolvePath path = do
  x <- get
  if isRelative path
    then return $ normalize $ getDirName (getLocation x) </> path
    else if getDirName (getRoot x) `isPrefixOf` path
           then return $ normalize path
           else throwError (FileSystemError "not found")

-- | Utility 'Command' that modifies state setting current directory to root.
toRoot :: Command ()
toRoot = do
  x <- get
  put $ FileSystem (getRoot x) (getRoot x)

-- | Utility 'Command' that replaces a 'Dir' in it's parent with new value.
propagateOnce :: Command FileSystem
propagateOnce = do
  x <- get
  let loc = getLocation x
  y <- relocateBack
  let dir = Dir (getDirName $ getLocation y) $
            M.insert (getDirName loc) (D loc) (getItems $ getLocation y)
  move dir

-- | Utility 'Command' that performs 'propagateOnce' until the root is reached.
propagate :: Command ()
propagate = do
  x <- get
  if getDirName (getRoot x) /= getDirName (getLocation x)
    then do
      _ <- propagateOnce
      propagate
    else do
      y <- get
      put $ FileSystem (getLocation y) (getLocation y)

-- | 'Command' modifier that performs given action using 'propagate' to save
-- all new data and than returns working directory to it's original path.
corrected :: Command a -> Command FileSystem
corrected action = do
  x <- get
  _ <- action
  propagate
  relocate $ getDirName $ getLocation x

-- | Utility for 'touch' command that performs file creation and uses
-- 'propagate' to save all updates.
createFile :: FilePath -> BS.ByteString -> Command FileSystem
createFile path content =
  corrected $ do
    let loc = cutLast path
    y <- relocate loc
    let m = getItems $ getLocation y
    case M.lookup path m of
      Just _ -> throwError $ FileSystemError "name already exists"
      Nothing -> do
        let dir = Dir (getDirName $ getLocation y) (M.insert path (F $ File path content) m)
        move dir

-- | Utility for 'mkdir' command that performs directory creation and uses
-- 'propagate' to save all updates.
createDir :: FilePath -> Command FileSystem
createDir path =
  corrected $ do
    let loc = cutLast path
    y <- relocate loc
    let m = getItems $ getLocation y
    case M.lookup path m of
      Just _ -> throwError $ FileSystemError "name already exists"
      Nothing -> do
        let dir = Dir (getDirName $ getLocation y) (M.insert path (D $ Dir path M.empty) m)
        move dir

-- | Instance of 'Parser' that parses 'FilePath' argument.
pathParser :: Parser Char FilePath
pathParser = (stream "\"" *> many (p <|> element ' ') <* stream "\"") <|> many p
  where
    p :: Parser Char Char
    p = notP $ element '\"'

-- | Instance of 'Parser' that parses 'Command' corresponding to given
-- command name and arguments.
callParser :: Parser Char Call
callParser =
  (stream "ls" $> (Call . ls) <* many (element ' ') <*> pathParser) <|>
  (stream "cd" $> (Call . cd) <* many (element ' ') <*> pathParser) <|>
  (stream "touch" $> (Call . touch) <* many (element ' ') <*> pathParser) <|>
  (stream "mkdir" $> (Call . mkdir) <* many (element ' ') <*> pathParser) <|>
  (stream "cat" $> (Call . cat) <* many (element ' ') <*> pathParser) <|>
  (stream "exit" $> Exit)

-- | Utility function that works as 'words' and 'filter', in other words
-- that splits a 'String' by given predicate on 'Char's.
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsBy p s''
      where (w, s'') = break p s'

-- | Utility function for 'FilePath' separation into directories'
-- and files' names that drops the common part of tho arguments before it.
cutPath :: FilePath -> FilePath -> [FilePath]
cutPath root path = wordsBy (== pathSeparator) $ drop (length root) path

-- | Utility function for 'FilePath' separation that drops all
-- parent directories and leaves only short name.
cutLast :: FilePath -> FilePath
cutLast path = snd $ cutInner path
  where
    cutInner :: FilePath -> (Bool, FilePath)
    cutInner "" = (False, "")
    cutInner (x:xs) =
      case cutInner xs of
        (False, "") -> (x == pathSeparator, "")
        (_, rs) -> (True, x : rs)

-- | Utility function. Creates a 'Command' that changes current directory
-- safely by performing one step into neighbouring directory.
relocateOnce :: FilePath -> Command FileSystem
relocateOnce ".." = relocateBack
relocateOnce "." = get
relocateOnce "" = get
relocateOnce path = do
  x <- get
  let curPath = getDirName $ getLocation x
  case M.lookup (curPath </> path) (getItems $ getLocation x) of
    Nothing -> throwError $ FileSystemError "not found"
    Just (F _) -> throwError $ FileSystemError "not a directory"
    Just (D d) -> move d

-- | Utility function. Creates a 'Command' that changes current directory
-- by safely performing 'relocateOnce' multiple times.
relocateMany :: [FilePath] -> Command FileSystem
relocateMany = foldr ((>>) . relocateOnce) get

-- | Utility 'Command' that safely changes current directory into it's parent.
relocateBack :: Command FileSystem
relocateBack = do
  x <- get
  let moves = take (length a - 1) a
        where
          a = cutPath (getDirName $ getRoot x) (getDirName $ getLocation x)
  result <- toRoot >> relocateMany moves
  put result
  return result

-- | Utility function. Creates a 'Command' that wraps 'relocateBack'
-- and 'relocateMany' to perform both absolute and relative path changes.
relocate :: FilePath -> Command FileSystem
relocate path = do
  x <- get
  let root = getDirName $ getRoot x
  y <-
    if isRelative path
      then relocateMany $ cutPath "" path
      else if root `isPrefixOf` path
             then toRoot >> relocateMany (cutPath root path)
             else return x >> throwError (FileSystemError "not found")
  put y
  return y

-- | Command line 'ls' command. Takes an optional 'FilePath' argument
-- and lists given directory's contents.
ls :: FilePath -> Command ()
ls path = do
  x <- get
  y <- relocate path
  let getShortName z = last (wordsBy (== pathSeparator) (getName z))
  liftIO $ putStr (foldr (\z t -> getShortName z ++ "\n" ++ t) "" (getItems $ getLocation y))
  _ <- move $ getLocation x
  return ()

-- | Command line 'cd' command. Takes a 'FilePath' argument and changes current
-- directory into given one.
cd :: FilePath -> Command ()
cd path = do
  _ <- relocate path
  return ()

-- | Command line 'touch' command. Takes a 'FilePath' argument and creates
-- an empty file on given path.
touch :: FilePath -> Command ()
touch path = do
  p <- resolvePath path
  x <- createFile p BS.empty
  put x

-- | Command line 'mkdir' command. Takes a 'FilePath' argument and creates
-- an empty directory on given path.
mkdir :: FilePath -> Command ()
mkdir path = do
  p <- resolvePath path
  x <- createDir p
  put x

-- | Command line 'cat' command. Takes a 'FilePath' and outputs given
-- file's content.
cat :: FilePath -> Command ()
cat path = do
  p <- resolvePath path
  let loc = cutLast p
  y <- relocate loc
  let m = getItems $ getLocation y
  case M.lookup p m of
    Nothing -> throwError $ FileSystemError "no such file"
    Just (D _) -> throwError $ FileSystemError "not a file"
    Just (F file) -> liftIO $ putStrLn (unpack $ getData file)