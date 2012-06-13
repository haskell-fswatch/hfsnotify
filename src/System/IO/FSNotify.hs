--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify
       ( ActionPredicate
       , Action
       , Event(..)
       , FileListener(..)
       , act
       ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)

data Event =
    Added    FilePath
  | Modified FilePath
  | Removed  FilePath
  deriving (Show)

type ActionPredicate = Event -> Bool
type Action = Event -> IO ()

act :: ActionPredicate
act event = True

class FileListener sessionType where
  initSession :: IO sessionType
  killSession :: sessionType -> IO ()
  listen  :: sessionType -> FilePath -> ActionPredicate -> Action -> IO ()
  rlisten :: sessionType -> FilePath -> ActionPredicate -> Action -> IO ()
