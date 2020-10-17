{- |
Module      : FSError
Description : Composite File System Error class
  
Error consisting of 'FileSystemError' and 'ParserError'.
-}
module FSError
  ( FSError(..)
  ) where

import Control.Exception (Exception)

-- | Data class 'FSError' represents any error occurrence during execution.
-- It has constructors 'FilesSystemError' representing error during 
-- communication with FS and 'ParserError' representing parsing error. 
data FSError
  = FileSystemError String
  | ParserError String
  deriving (Eq, Show)

instance Exception FSError