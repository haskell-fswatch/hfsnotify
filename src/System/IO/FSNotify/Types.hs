--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Types
       ( Event(..)
       , eventPath
       , eventTime
       , EventChannel
       , ActionPredicate
       , Action
       , act
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Data.Time ()
import Data.Time.Clock (UTCTime)
import Filesystem.Path.CurrentOS

-- | A file event reported by a file watcher.
data Event =
    Added    FilePath UTCTime
  | Modified FilePath UTCTime
  | Removed  FilePath UTCTime
  deriving (Show)

-- | Helper for extracting the path associated with an event.
eventPath :: Event -> FilePath
eventPath (Added    path _) = path
eventPath (Modified path _) = path
eventPath (Removed  path _) = path

-- | Helper for extracting the time associated with an event.
eventTime :: Event -> UTCTime
eventTime (Added    _ timestamp) = timestamp
eventTime (Modified _ timestamp) = timestamp
eventTime (Removed  _ timestamp) = timestamp

type EventChannel = Chan Event

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
