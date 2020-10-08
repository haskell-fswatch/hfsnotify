--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Types
       ( act
       , ActionPredicate
       , Action
       , WatchConfig(..)
       , Debounce(..)
       , DebounceData(..)
       , DebouncePayload
       , Event(..)
       , EventIsDirectory(..)
       , EventChannel
       , IOEvent
       ) where

import Control.Concurrent.Chan
import Data.IORef (IORef)
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime)
import Prelude hiding (FilePath)
import System.FilePath

data EventIsDirectory = IsFile | IsDirectory
  deriving (Show, Eq)

-- | A file event reported by a file watcher. Each event contains the
-- canonical path for the file and a timestamp guaranteed to be after the
-- event occurred (timestamps represent current time when FSEvents receives
-- it from the OS and/or platform-specific Haskell modules).
data Event =
    Added { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  | Modified { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  | Removed { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  | WatchedDirectoryRemoved  { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
    -- ^ Note: currently only emitted on Linux
  | Unknown  { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory, eventString :: String }
  deriving (Eq, Show)

type EventChannel = Chan Event

-- | Watch configuration
data WatchConfig = WatchConfig
  { confDebounce :: Debounce
    -- ^ Debounce configuration
  , confPollInterval :: Int
    -- ^ Polling interval if polling is used (microseconds)
  , confUsePolling :: Bool
    -- ^ Force use of polling, even if a more effective method may be
    -- available. This is mostly for testing purposes.
  , confThreadPerEvent :: Bool
    -- ^ Fork a separate thread for each event when executing the
    -- user supplied event handler
  }

-- | This specifies whether multiple events from the same file should be
-- collapsed together, and how close is close enough.
--
-- This is performed by ignoring any event that occurs to the same file
-- until the specified time interval has elapsed.
--
-- Note that the current debouncing logic may fail to report certain changes
-- to a file, potentially leaving your program in a state that is not
-- consistent with the filesystem.
--
-- Make sure that if you are using this feature, all changes you make as a
-- result of an 'Event' notification are both non-essential and idempotent.
data Debounce
  = DebounceDefault
    -- ^ perform debouncing based on the default time interval of 1 millisecond
  | Debounce NominalDiffTime
    -- ^ perform debouncing based on the specified time interval
  | NoDebounce
    -- ^ do not perform debouncing

type IOEvent = IORef Event

-- | DebouncePayload contents. Contains epsilon value for debouncing
-- near-simultaneous events and an IORef of the latest Event. Difference in
-- arrival time is measured according to Event value timestamps.
data DebounceData = DebounceData NominalDiffTime IOEvent

-- | Data "payload" passed to event handlers to enable debouncing. This value
-- is automatically derived from a 'WatchConfig' value. A value of Just
-- DebounceData results in debouncing according to the given epsilon and
-- IOEvent. A value of Nothing results in no debouncing.
type DebouncePayload = Maybe DebounceData

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action m = Event -> m ()

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
