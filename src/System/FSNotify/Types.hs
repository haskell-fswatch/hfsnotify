--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP #-}

module System.FSNotify.Types (
  act
  , ActionPredicate
  , Action
  , DebounceFn
  , WatchConfig(..)
  , WatchMode(..)
  , ThreadingMode(..)
  , Event(..)
  , EventIsDirectory(..)
  , AddedExtraInfo(..)
  , RescanReason(..)
  , EventCallback
  , EventChannel
  , EventAndActionChannel
  , IOEvent
  ) where

import Control.Concurrent.Chan
import Control.Exception.Safe
import Data.IORef (IORef)
import Data.Time.Clock (UTCTime)
import Prelude hiding (FilePath)
import System.FilePath

data EventIsDirectory = IsFile | IsDirectory
  deriving (Show, Eq)

-- | How a path came to be at the location reported by an 'Added' event, when the backend
-- is able to tell.
data AddedExtraInfo =
  -- | The path was created in place, e.g. by @open@ with @O_CREAT@ or by @mkdir@. A file
  -- reported this way may still be open for writing, so its contents may be incomplete.
  --
  -- Emitted by the Linux and FreeBSD backends (inotify's @IN_CREATE@) and the macOS backend
  -- (FSEvents' @kFSEventStreamEventFlagItemCreated@). On Linux and FreeBSD, you can wait for
  -- the file's 'CloseWrite' event to know that writing has finished.
    AddedByCreate
  -- | The path was moved or renamed into place, e.g. by @rename@. Since the file was written
  -- elsewhere, its contents are already complete, and no 'CloseWrite' event will follow.
  --
  -- Emitted by the Linux and FreeBSD backends (inotify's @IN_MOVED_TO@) and the macOS backend
  -- (FSEvents' @kFSEventStreamEventFlagItemRenamed@).
  | AddedByMove
  -- | The backend can't tell how the path came to be.
  --
  -- Emitted by the Windows backend (which reports creations and move-ins identically), by the
  -- polling backend on every platform, and by the Linux and FreeBSD backends for events they
  -- synthesize for files found in a newly created directory when watching recursively.
  | AddedNoExtraInfo
  deriving (Show, Eq)

-- | A file event reported by a file watcher. Each event contains the
-- canonical path for the file and a timestamp guaranteed to be after the
-- event occurred (timestamps represent current time when FSEvents receives
-- it from the OS and/or platform-specific Haskell modules).
data Event =
    Added { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory, eventAddedExtraInfo :: AddedExtraInfo }
  | Modified { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  | ModifiedAttributes { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  | Removed { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  -- | Note: Linux-only
  | WatchedDirectoryRemoved  { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  -- | Note: Linux-only
  | CloseWrite  { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory }
  -- | Note: Linux-only
  | Unknown  { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory, eventString :: String }
  -- | Events affecting the directory 'eventPath' were lost, so it should be rescanned: recursively
  -- for a recursive watch, and just its entries for a non-recursive one.
  | Rescan { eventPath :: FilePath, eventTime :: UTCTime, eventIsDirectory :: EventIsDirectory, eventRescanReason :: RescanReason }
  deriving (Eq, Show)

-- | Why a 'Rescan' was needed
data RescanReason =
  -- | macOS: FSEvents coalesced events under the path into one
  RescanCoalesced
  -- | macOS: fseventsd dropped events because this process didn't keep up
  | RescanUserDropped
  -- | macOS: the kernel dropped events because fseventsd didn't keep up
  | RescanKernelDropped
  -- | Linux: the inotify event queue overflowed
  | RescanQueueOverflow
  deriving (Eq, Show)

type EventChannel = Chan Event

type EventCallback = Event -> IO ()

type EventAndActionChannel = Chan (Event, Action)

-- | Method of watching for changes.
data WatchMode =
  WatchModePoll {
    watchModePollInterval :: Int
    -- ^ Polling interval in microseconds.
  }
  -- ^ Detect changes by polling the filesystem. Less efficient and may miss fast changes. Not recommended
  -- unless you're experiencing problems with 'WatchModeOS' (or 'WatchModeOS' is not supported on your platform).
#ifdef HAVE_NATIVE_WATCHER
  | WatchModeOS
  -- ^ Use OS-specific mechanisms to be notified of changes (inotify on Linux, FSEvents on OSX, etc.).
  -- Not currently available on e.g. *BSD and Wasm/WASI.
#endif

data ThreadingMode =
  SingleThread
  -- ^ Use a single thread for the entire 'Manager'. Event handler callbacks will run sequentially.
  | ThreadPerWatch
  -- ^ Use a single thread for each watch (i.e. each call to 'watchDir', 'watchTree', etc.).
  -- Callbacks within a watch will run sequentially but callbacks from different watches may be interleaved.
  | ThreadPerEvent
  -- ^ Launch a separate thread for every event handler.

-- | Watch configuration.
data WatchConfig = WatchConfig
  { confWatchMode :: WatchMode
    -- ^ Watch mode to use.
  , confThreadingMode :: ThreadingMode
    -- ^ Threading mode to use.
  , confOnHandlerException :: SomeException -> IO ()
    -- ^ Called when a handler throws an exception or a watch fails internally
  }

type IOEvent = IORef Event

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | A general debouncing function.
type DebounceFn = Action -> IO Action

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
