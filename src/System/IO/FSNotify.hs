--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | A cross-platform file watching mechanism.

module System.IO.FSNotify
       ( startManager
       , stopManager
       , watchDirChan
       , watchDirAction
       , watchTreeChan
       , watchTreeAction
       , WatchManager
       ) where

import Prelude hiding (FilePath, catch)

import Control.Concurrent
import Control.Exception
import Data.Map (Map)
import Filesystem.Path.CurrentOS
import System.IO.FSNotify.Polling
import System.IO.FSNotify.Types
import qualified Data.Map as Map

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

-- | Watch the immediate contents of a directory by streaming events to a Chan.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDirChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
watchDirChan (WatchManager wm) = case wm of
  Right native -> listen native
  Left  poll   -> listen poll

-- | Watch all the contents of a directory by streaming events to a Chan.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTreeChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
watchTreeChan (WatchManager wm) = case wm of
  Right native -> rlisten native
  Left  poll   -> rlisten poll

-- | Watch the immediate contents of a directory by committing an Action for each event.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories. No two events pertaining to the same FilePath will
-- be executed concurrently.
watchDirAction :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO ()
watchDirAction (WatchManager wm) path actPred action = case wm of
  Right native -> do
    chan <- newChan
    _    <- forkIO $ readEvents chan action Map.empty
    listen native path actPred chan
  Left  poll   ->  do
    chan <- newChan
    _    <- forkIO $ readEvents chan action Map.empty
    listen poll path actPred chan

-- | Watch all the contents of a directory by committing an Action for each event.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories. No two events
-- pertaining to the same FilePath will be executed concurrently.
watchTreeAction :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO ()
watchTreeAction (WatchManager wm) path actPred action = case wm of
  Right native -> do
    chan <- newChan
    _    <- forkIO $ readEvents chan action Map.empty
    rlisten native path actPred chan
  Left  poll   -> do
    chan <- newChan
    _    <- forkIO $ readEvents chan action Map.empty
    rlisten poll path actPred chan

type ThreadLock = MVar ()
type PathLockMap = Map FilePath ThreadLock

readEvents :: EventChannel -> Action -> PathLockMap -> IO ()
readEvents chan action  pathMap = do
  event <- readChan chan
  let path = eventPath event
  mVar <- getMVar $ Map.lookup path pathMap
  _ <- takeMVar mVar >> (forkIO $ action event  `finally` putMVar mVar ())
  readEvents chan action  $ Map.insert path mVar pathMap
  where
    getMVar :: Maybe ThreadLock -> IO ThreadLock
    getMVar (Just tl) = return tl
    getMVar Nothing   = newMVar ()

{-
readEvents :: EventChannel -> Action -> IO ()
readEvents chan action = do
  event <- readChan chan
  _     <- forkIO $ action event
  readEvents chan action
-}
