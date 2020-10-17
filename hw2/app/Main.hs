{-# LANGUAGE FlexibleContexts #-}

module Main
  ( commandLoop
  , main
  ) where

import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT, get, put)

import System.Directory (setCurrentDirectory)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Commands
import FileSystem (dumpFileSystem, getDirName, getLocation, readFileSystem)
import Parser.Parser

-- | Function iterating in an infinite loop of 'Call's and catching errors
-- until 'Exit' command is met.
commandLoop :: Command ()
commandLoop = do
  x <- get
  liftIO . putStr $ (getDirName . getLocation $ x) ++ "> "
  cmd <- liftIO getLine
  let fixError e = do
        liftIO $ print e
        y <- move (getLocation x)
        put y
  case runParser callParser cmd of
    Right (Exit, _) -> return ()
    Right (Call command, _) -> do
      catchError command $ \e -> fixError e
      commandLoop
    Left e -> do
      fixError e
      commandLoop

-- | Main function that starts 'commandLoop' on read 'FileSystem' and
-- dumps resulting 'FileSystem' in the end.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter starting directory: "
  path <- getLine
  _ <- setCurrentDirectory path
  r <- readFileSystem
  fs <- execStateT (runExceptT commandLoop) r
  _ <- dumpFileSystem fs
  return ()