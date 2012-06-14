--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.FSNotify.Types
       ( ListenUnsupportedException
       , Event(..)
       , ActionPredicate
       , Action
       , act
       , FileListener(..)
       ) where

import Prelude hiding (FilePath, catch)

import Control.Exception
import Data.Typeable
import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)

data ListenUnsupportedException = ListenUnsupportedException deriving (Typeable, Show)
instance Exception ListenUnsupportedException

class FileListener sessionType where
  initSession :: IO sessionType
  killSession :: sessionType -> IO ()
  listen  :: sessionType -> FilePath -> ActionPredicate -> Action -> IO ()
  rlisten :: sessionType -> FilePath -> ActionPredicate -> Action -> IO ()

data Event =
    Added    FilePath
  | Modified FilePath
  | Removed  FilePath
  deriving (Show)

type ActionPredicate = Event -> Bool
type Action = Event -> IO ()

act :: ActionPredicate
act event = True
