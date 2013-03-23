--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Types
       ( act
       , ActionPredicate
       , Action
       , MkAction
       , WatchConfig(..)
       , Event(..)
       , AddModRem(..)
       , IsDir
       , EventChannel
       , isAddEvent, isModEvent, isExistsEvent, isRemEvent
       , isDirEvent, isFileEvent 
       , eventPath
       , eventTime
       , IOEvent
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Data.IORef (IORef)
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime)
import Filesystem.Path.CurrentOS

-- | A file event reported by a file watcher. Each event contains the
-- canonical path for the file and a timestamp guaranteed to be after the
-- event occurred (timestamps represent current time when FSEvents receives
-- it from the OS and/or platform-specific Haskell modules).
data Event = Event AddModRem FilePath UTCTime IsDir deriving (Eq, Show)
data AddModRem = Add | Mod | Rem deriving (Eq, Ord, Show)
type IsDir = Bool

-- instance of Ord will order things somewhat intelligently:
--   order by time, isDir, path, action
instance Ord Event where
    compare (Event act1 p1 tm1 dir1) (Event act2 p2 tm2 dir2) =
        case compare tm1 tm2 of
            EQ -> case compare dir2 dir1 of
                EQ -> case compare p1 p2 of
                    EQ -> compare act1 act2
                    p -> p
                d -> d
            t -> t

-- | Extract information about event type. 
isAddEvent, isModEvent, isExistsEvent, isRemEvent, isDirEvent, isFileEvent :: Event -> Bool
isAddEvent (Event Add _ _ _) = True
isAddEvent _ = False
isModEvent (Event Mod _ _ _) = True
isModEvent _ = False
isRemEvent (Event Rem _ _ _) = True
isRemEvent _ = False
isExistsEvent = not . isRemEvent
isDirEvent (Event _ _ _ b) = b
isFileEvent = not . isDirEvent

-- | Helper for extracting the path associated with an event.
eventPath :: Event -> FilePath
eventPath (Event _ path _ _) = path

-- | Helper for extracting the time associated with an event.
eventTime :: Event -> UTCTime
eventTime (Event _ _ timestamp _) = timestamp

type EventChannel = Chan Event

-- | Config object, currently used just for debouncing events.
--
-- To configure in a future-compatible way, use defaultConfig and
-- add only known properties.
--
--    defaultConfig { debounce = 0.002 }
--
-- The properties are currently:
--
--   debounce - ignore subsequent events on a path if they occur in
--     less than the debounce time. If debounce is 0 or less, it is
--     disabled.
--
data WatchConfig = WatchConfig 
    { debounce :: NominalDiffTime
    }

type IOEvent = IORef Event

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | For recursive observation, we may need an action per directory.
-- Here the FilePath is the parent directory, while the events on the
-- constructed action are for files or immediate child directories.
type MkAction = FilePath -> IO Action

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
