--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | A cross-platform file watching mechanism.

module System.IO.FSNotify
       ( startManager
       , stopManager
       , watch
       , rwatch
       , WatchManager
       ) where

import Prelude hiding (catch)

import Control.Exception
import System.IO.FSNotify.Polling
import System.IO.FSNotify.Types

#ifdef OS_Linux
import System.IO.FSNotify.Linux
#else
#  ifdef OS_Win32
import System.IO.FSNotify.Win32
#  else
#    ifdef OS_Mac
import System.IO.FSNotify.OSX
#    else
type NativeManager = PollManager
#    endif
#  endif
#endif

data WatchManager = WatchManager (Either PollManager NativeManager)

createManager :: Maybe NativeManager -> IO WatchManager
createManager (Just nativeManager) = return $ WatchManager $ Right nativeManager
createManager Nothing = createPollManager >>= return . WatchManager . Left

-- | Start a file watch manager.
-- Directories can only be watched when they are managed by an started watch
-- watch manager.
startManager :: IO WatchManager -- ^ The watch manager. Use this to watch directories and clean up when done.
startManager = initSession >>= createManager

-- | Stop a file watch manager.
-- Stopping a watch manager will immediately stop processing events on all paths
-- being watched using the manager.
stopManager :: WatchManager -> IO ()
stopManager (WatchManager wm) =
  case wm of
    Right native -> killSession native
    Left poll    -> killSession poll

-- | Watch the immediate contents of a directory.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watch (WatchManager wm) = case wm of
    Right native -> listen native
    Left poll    -> listen poll

-- | Watch all the contents of a directory.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
rwatch (WatchManager wm) = case wm of
    Right native -> rlisten native
    Left poll    -> rlisten poll
