--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify
  ( FilePattern
  , Action
  , Event(..)
  , ListenerSession(..)
  , FileListener(..)
  ) where

import System.IO

type FilePattern = String

data Event =
    Created
      { filePath :: FilePath }
  | Modified
      { maybeFilePath :: Maybe FilePath }
  | Deleted
      { filePath :: FilePath }
type Action = Event -> IO ()

class ListenerSession sessionType where
  initSession :: IO sessionType
  killSession :: sessionType -> IO ()

class FileListener sessionType handleType where
  listen  :: sessionType -> FilePath -> FilePattern -> Action -> IO  handleType
  rlisten :: sessionType -> FilePath -> FilePattern -> Action -> IO [handleType]
