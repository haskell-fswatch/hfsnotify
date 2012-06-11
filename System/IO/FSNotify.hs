--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}

module System.IO.FSNotify
       ( ActionPredicate
       , Action
       , Event(..)
       , ListenerSession(..)
       , FileListener(..)
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

class ListenerSession sessionType where
  initSession :: IO sessionType
  killSession :: sessionType -> IO ()

class FileListener sessionType handleType where
  listen  :: sessionType -> FilePath -> ActionPredicate -> Action -> IO  handleType
  rlisten :: sessionType -> FilePath -> ActionPredicate -> Action -> IO [handleType]
