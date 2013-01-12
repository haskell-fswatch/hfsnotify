--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | cross-platform file watching.

module System.FSNotify
       ( Event(..)
       , EventChannel
       , Action
       , ActionPredicate

       -- * Starting/Stopping
       , WatchManager
       , withManager
       , startManager
       , stopManager
       , defaultConfig
       , WatchConfig(..)
       , withManagerConf
       , startManagerConf

       -- * Watching
       , WatchDescriptor
       , removeWatch
       , watchDir
       , watchDirChan
       , watchTree
       , watchTreeChan
       ) where

import Prelude hiding (FilePath, catch)

import Control.Concurrent
import Control.Exception
import Data.Map (Map)
import Filesystem.Path.CurrentOS
import System.FSNotify.Polling
import System.FSNotify.Types
import qualified Data.Map as Map

#ifdef OS_Linux
import System.FSNotify.Linux
#else
#  ifdef OS_Win32
import System.FSNotify.Win32
#  else
#    ifdef OS_Mac
import System.FSNotify.OSX
#    else
type NativeManager = PollManager
#    endif
#  endif
#endif

data WatchManager = WatchManager WatchConfig (Either PollManager NativeManager)
data WatchDescriptor = NativeWD (WatchID NativeManager) | PollWD (WatchID PollManager)

defaultConfig :: WatchConfig
defaultConfig = DebounceDefault

-- | Perform an IO action with a WatchManager in place.
-- Tear down the WatchManager after the action is complete.
withManager :: (WatchManager -> IO a) -> IO a
withManager  = withManagerConf defaultConfig

-- | Start a file watch manager.
-- Directories can only be watched when they are managed by a started watch
-- watch manager.
-- When finished watching. you must release resources via 'stopManager'.
-- It is preferrable if possible to use 'withManager' to handle this
-- automatically.
startManager :: IO WatchManager
startManager = startManagerConf defaultConfig

-- | Stop a file watch manager.
-- Stopping a watch manager will immediately stop
-- watching for files and free resources.
stopManager :: WatchManager -> IO ()
stopManager (WatchManager _ wm) =
  case wm of
    Right native -> killSession native
    Left poll    -> killSession poll

withManagerConf :: WatchConfig -> (WatchManager -> IO a) -> IO a
withManagerConf debounce = bracket (startManagerConf debounce) stopManager

startManagerConf :: WatchConfig -> IO WatchManager
startManagerConf debounce = initSession >>= createManager
  where
    createManager :: Maybe NativeManager -> IO WatchManager
    createManager (Just nativeManager) = return (WatchManager debounce (Right nativeManager))
    createManager Nothing = return . (WatchManager debounce) . Left =<< createPollManager


-- Helper function for all watch* functions.
watch :: (FileListener sessionType)
      => ((WatchID sessionType) -> WatchDescriptor)
      -> (sessionType -> FilePath -> ActionPredicate -> evtChanOrAction -> IO (WatchID sessionType))
      -> sessionType
      -> FilePath
      -> ActionPredicate
      -> evtChanOrAction
      -> IO WatchDescriptor
watch wdCtor listener wm fp ap eca = listener wm fp ap eca >>= return . wdCtor

-- Helper functions for dealing with Either PollManager NativeManager type.
watchPoll :: (PollManager -> FilePath -> ActionPredicate -> evtChanOrAction -> IO (WatchID PollManager))
          -> PollManager
          -> FilePath
          -> ActionPredicate
          -> evtChanOrAction
          -> IO WatchDescriptor
watchPoll   = watch PollWD
watchNative :: (NativeManager -> FilePath -> ActionPredicate -> evtChanOrAction -> IO (WatchID NativeManager))
            -> NativeManager
            -> FilePath
            -> ActionPredicate
            -> evtChanOrAction
            -> IO WatchDescriptor
watchNative = watch NativeWD

-- | Watch the immediate contents of a directory by streaming events to a Chan.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDirChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO WatchDescriptor
watchDirChan (WatchManager db wm) =
  either (watchPoll (listen db)) (watchNative (listen db)) wm

-- | Watch all the contents of a directory by streaming events to a Chan.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTreeChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO WatchDescriptor
watchTreeChan (WatchManager db wm) =
  either (watchPoll (listenRecursive db)) (watchNative (listenRecursive db)) wm

-- | Watch the immediate contents of a directory by committing an Action for each event.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories. No two events pertaining to the same FilePath will
-- be executed concurrently.
watchDir :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO WatchDescriptor
watchDir (WatchManager db wm) = either (watchPoll runFallback) (watchNative runNative) wm
  where
    runFallback = threadChanFallback $ listen db
    runNative   = threadChanNative   $ listen db

removeWatch :: WatchManager -> WatchDescriptor -> IO ()
removeWatch (WatchManager _ wm) = either rwFallback rwNative wm
  where
    rwFallback :: PollManager -> WatchDescriptor -> IO ()
    rwFallback wm' (PollWD wd) = killListener wm' wd
    rwFallback _   _           = return ()
    rwNative :: NativeManager -> WatchDescriptor -> IO ()
    rwNative wm' (NativeWD wd) = killListener wm' wd
    rwNative _   _             = return ()

threadChanNative :: (NativeManager -> FilePath -> ActionPredicate -> Chan Event -> IO b) -> NativeManager -> FilePath -> ActionPredicate -> Action -> IO b
threadChanNative listener iface path actPred action =
      threadChan action $ listener iface path actPred

threadChanFallback :: (PollManager -> FilePath -> ActionPredicate -> Chan Event -> IO b) -> PollManager -> FilePath -> ActionPredicate -> Action -> IO b
threadChanFallback listener iface path actPred action =
      threadChan action $ listener iface path actPred

threadChan :: Action -> (Chan Event -> IO b) -> IO b
threadChan action runListener = do
      chan <- newChan
      _    <- forkIO $ readEvents chan action Map.empty
      runListener chan


-- | Watch all the contents of a directory by committing an Action for each event.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories. No two events
-- pertaining to the same FilePath will be executed concurrently.
watchTree :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO WatchDescriptor
watchTree (WatchManager db wm) = either (watchPoll runFallback) (watchNative runNative) wm
  where
    runFallback = threadChanFallback $ listenRecursive db
    runNative   = threadChanNative   $ listenRecursive db

type ThreadLock = MVar ()
type PathLockMap = Map FilePath ThreadLock

readEvents :: EventChannel -> Action -> PathLockMap -> IO ()
readEvents chan action  pathMap = do
  event <- readChan chan
  let path = eventPath event
  mVar <- getMVar $ Map.lookup path pathMap
  _ <- takeMVar mVar >> (forkIO $ action event `finally` putMVar mVar ())
  readEvents chan action  $ Map.insert path mVar pathMap
  where
    getMVar :: Maybe ThreadLock -> IO ThreadLock
    getMVar (Just tl) = return tl
    getMVar Nothing   = newMVar ()
