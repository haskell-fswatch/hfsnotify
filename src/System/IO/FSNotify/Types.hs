--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Types
       ( Event(..)
       , eventPath
       , EventChannel
       , ActionPredicate
       , Action
       , act
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Filesystem.Path.CurrentOS

-- | A file event reported by a file watcher.
data Event =
    Added    FilePath
  | Modified FilePath
  | Removed  FilePath
  deriving (Show)

-- | Helper for extracting the path associated with an event.
eventPath :: Event -> FilePath
eventPath (Added    path) = path
eventPath (Modified path) = path
eventPath (Removed  path) = path

type EventChannel = Chan Event

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
