module Spec
  ( fsTestTree
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, execStateT)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec, describe)

import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, setCurrentDirectory)
import System.FilePath ((</>))

import Commands
import FileSystem

fsTestTree :: IO TestTree
fsTestTree = do
  x <- fsSpec
  testSpec "filesystem" x

data Entity_
  = D_ Dir_
  | F_ File_
  deriving (Eq, Show)

data Dir_ =
  Dir_ String [Entity_]
  deriving (Eq, Show)

data File_ =
  File_ String String
  deriving (Eq, Show)

readAll_ :: FilePath -> IO Dir_
readAll_ path = do
  setCurrentDirectory path
  readDir_ path

readFile_ :: FilePath -> IO File_
readFile_ path = do
  content <- readFile path
  return $ File_ path content

readDir_ :: FilePath -> IO Dir_
readDir_ path = do
  items <- listDirectory path
  children <- listEntities path items
  return $ Dir_ path children
  where
    listEntities :: FilePath -> [FilePath] -> IO [Entity_]
    listEntities _ [] = return []
    listEntities from (x:xs) = do
      isFile <- doesFileExist (from </> x)
      next <- listEntities from xs
      if isFile
        then do
          first <- readFile_ (from </> x)
          return $ F_ first : next
        else do
          first <- readDir_ (from </> x)
          return $ D_ first : next

--fromFile :: File -> File_
--fromFile file = File_ (getFileName file) (unpack $ getData file)
--
--fromDir :: Dir -> Dir_
--fromDir dir = Dir_ (getDirName dir) (map toEntity_ $ M.toAscList $ getItems dir)
--
--toEntity_ :: (FilePath, Entity) -> Entity_
--toEntity_ (path, entity) =
--  case entity of
--    F file -> F_ $ fromFile file
--    D dir -> D_ $ fromDir dir
--
--fromFileSystem :: FileSystem -> Dir_
--fromFileSystem = fromDir . getRoot

test :: FilePath -> Command () -> IO Dir_
test path command = do
  createDirectoryIfMissing True path
  setCurrentDirectory path
  fs <- readFileSystem
  _ <- execStateT (runExceptT command) fs
  _ <- dumpFileSystem fs
  readAll_ path

testFails :: FilePath -> Command () -> IO Bool
testFails path command = do
  createDirectoryIfMissing True path
  setCurrentDirectory path
  fs <- readFileSystem
  res <- evalStateT (runExceptT command) fs
  return $
    case res of
      (Left _) -> True
      (Right _) -> False

fsSpec :: IO Spec
fsSpec =
  lsSpec

-- | Tests are working only on my device, sorry :(
-- Didn't have enough time to write proper temp-directory creation and 
-- random-filling.
--
-- Still, there're only tests for ls which don't even check what it outputs..
lsSpec :: IO Spec
lsSpec = do
  lsDot <- test "/home/dan/play_" (ls "")
  lsPar <- test "/home/dan/play_" (ls "..")
  lsA <- test "/home/dan/play_" (ls "a")
  lsB <- testFails "/home/dan/play_" (ls "b")
  lsX <- testFails "/home/dan/play_" (ls "X")
  return $
    describe "ls" $ do
      it "lsDot" $
        shouldBe lsDot $
        Dir_ "/home/dan/play_" [D_ $ Dir_ "/home/dan/play_/a" [], F_ $ File_ "/home/dan/play_/x" "aba\n"]
      it "lsPar" $
        shouldBe lsPar $
        Dir_ "/home/dan/play_" [D_ $ Dir_ "/home/dan/play_/a" [], F_ $ File_ "/home/dan/play_/x" "aba\n"]
      it "lsA" $
        shouldBe lsA $ Dir_ "/home/dan/play_" [D_ $ Dir_ "/home/dan/play_/a" [], F_ $ File_ "/home/dan/play_/x" "aba\n"]
      it "lsB" $ shouldBe lsB True
      it "lsX" $ shouldBe lsX True

--cdSpec :: IO Spec
--cdSpec =
--  return $ do
--    it "fake1" $ shouldBe "x" "x"
--    it "fake2" $ shouldBe "x" "Y"