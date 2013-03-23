--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | cross-platform file watching.
module System.FSNotify
       ( Event(..)
       , AddModRem(..)
       , IsDir
       , EventChannel
       , ActionPredicate
       , Action
       , MkAction

       -- * Starting/Stopping
       , WatchManager
       , withManager
       , startManager
       , stopManager
       , WatchConfig(..)
       , defaultConfig
       , withManagerConf
       , startManagerConf

       -- * Watching
       , removeWatch
       , watchDir
       , watchDirChan
       , watchTree
       , watchTreeChan
       , watchTreeCtor
       , watchDir'
       , watchTreeCtor'

       -- * Event Structure
       , isAddEvent, isModEvent, isExistsEvent, isRemEvent
       , isDirEvent, isFileEvent, eventPath, eventTime
       ) where

import Prelude hiding (FilePath, catch)

import Control.Concurrent
import Control.Exception
import Control.Monad (join, when, void)
import Data.Map (Map)
import Data.IORef
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Types
import System.FSNotify.Debounce
import System.FSNotify.Path
import qualified Data.Map as Map

#if defined(OS_Linux)
import System.FSNotify.Linux
#elif defined(OS_Win32)
import System.FSNotify.Win32
#elif defined(OS_MAC)
import System.FSNotify.OSX
#elif defined(USE_POLLING)
import System.FSNotify.Polling
#endif

data WatchManager = WatchManager 
    { _wmConfig  :: WatchConfig   -- config data
    , _wmSession :: Session  -- native or polling
    , wmQueues  :: IORef (Map FilePath [IO ()]) -- queue per active dir
    }

defaultConfig :: WatchConfig
defaultConfig = WatchConfig 
    { debounce = 0.001
    }

-- | Perform an IO action with a WatchManager in place.
-- Tear down the WatchManager after the action is complete.
withManager :: (WatchManager -> IO a) -> IO a
withManager  = withManagerConf defaultConfig

-- | Start a new file watch manager.
--
-- Each watch manager can watch a set of directories, with at most
-- one action per directory. The 'one action per directory' limit
-- serves both simplicity and cross-platform compatibility.
--
-- Directories can only be watched when they are managed by a started
-- watch manager. 
--
-- You may unwatch specific directories, but when done with watching
-- you must releate resources via 'stopManager' (even if you never
-- start any watches). 
startManager :: IO WatchManager
startManager = startManagerConf defaultConfig

-- | Stop a file watch manager. 
--
-- Will halt future callbacks for all directories and free all OS
-- resources associated with the manager. You cannot reuse a manager
-- after stopping it, but you can start a new one.
--
-- Existing callbacks may still be in-progress. The client must be
-- robust to straggling updates.
stopManager :: WatchManager -> IO ()
stopManager (WatchManager _ wm fl) = killSession wm >> clearAllQueues fl

withManagerConf :: WatchConfig -> (WatchManager -> IO a) -> IO a
withManagerConf wc = bracket (startManagerConf wc) stopManager

startManagerConf :: WatchConfig -> IO WatchManager
startManagerConf wc = do
    wm <- newSession
    fl <- newIORef Map.empty
    return (WatchManager wc wm fl)

-- | Remove watch from a specific directory. This will halt future
-- callbacks. It will do nothing to stop callbacks in progress, so
-- the client must be robust to a few stragglers. Resources for the
-- particular watch will be cleaned up.
--
-- Note: if you're using tree-based recursive watches, you can
-- remove directories you aren't interested in (one at a time), but
-- if you want to recursively unwatch you should instead stopManager
-- and start a new one. (Recursive use of removeWatch is unreliable 
-- due to straggling updates and race conditions.) 
--
removeWatch :: WatchManager -> FilePath -> IO ()
removeWatch (WatchManager _ wm fl) dir = do
    dir' <- canonicalizeDirPath dir 
    clearWatch wm dir'
    clearQueue fl dir'

-- | Watch the immediate contents of a directory by streaming events to a Chan.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDirChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
watchDirChan wm dir pr = watchDir' wm dir pr . writeChan

-- | Watch all the contents of a directory by streaming events to a Chan.
-- Events are reported for files within the specified directory and its 
-- recursive subdirectories. Note that this consumes the watch actions 
-- for the entire tree (since a single WatchManager can only have one 
-- action per directory).
watchTreeChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
watchTreeChan wm dir pr = watchTreeCtor' wm dir pr . chanAction where
    chanAction = const . return . writeChan

-- | Watch the immediate contents of a directory by committing an Action 
-- for each event. Actions in this directory are serialized.
watchDir :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO ()
watchDir wm dir pr action = do
    dir' <- canonicalizeDirPath dir
    watchDir' wm dir pr (forkOnKey (wmQueues wm) dir' . action)

-- | Watch all the contents of a directory by committing an action 
-- for each event. Events are reported for files within the specified
-- directory and its recursive subdirectories. Note that this consumes
-- watch actions for the entire tree (since a single WatchManager can
-- only have one action per directory). Actions for each directory are
-- serialized, but different directories may operate concurrently.
watchTree :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO ()
watchTree wm dir pr action = watchTreeCtor wm dir pr (const $ return action)

-- | Watch contents of a directory, and recursively each subdirectory,
-- performing a directory-specific action that is constructed when the
-- subdirectory is located. Note that this consumes watch actions for
-- the entire tree (since a single WatchManager has only one action 
-- per directory). Actions within each directory are serialized, but
-- different directories may operate concurrently.
watchTreeCtor :: WatchManager -> FilePath -> ActionPredicate -> MkAction -> IO ()
watchTreeCtor wm dir pr mkAction = watchTreeCtor' wm dir pr mkAction' where
    mkAction' subDir = do
        innerAction <- mkAction subDir 
        return (forkOnKey (wmQueues wm) subDir . innerAction)

-- one thread per key, with ability to add tasks to a running thread
-- via the same key. When the thread finishes, the key is removed 
-- from the map atomically, and the next operation on the same key
-- will start a new thread.
forkOnKey :: (Ord k) => IORef (Map k [IO ()]) -> k -> IO () -> IO ()
forkOnKey rf k op = join $ atomicModifyIORef rf forkOrSched where
    forkOrSched m0 =
        case Map.lookup k m0 of
            Just ops -> (Map.insert k (ops ++ [op]) m0, return ()) -- sched op
            Nothing -> (Map.insert k [] m0, doFork) -- fork op
    doFork = void $ forkIO $ op >> keyProcLoop rf k

-- the loop action of the thread that handles forkOnKey tasks.
keyProcLoop :: (Ord k) => IORef (Map k [IO ()]) -> k -> IO ()
keyProcLoop rf k = join $ atomicModifyIORef rf takeOp where
    takeOp m0 = takeOp' m0 (Map.lookup k m0)
    takeOp' m0 Nothing = assert False $ (m0,return()) -- illegal state; thread forgotten!
    takeOp' m0 (Just []) = let m' = Map.delete k m0 in (m', m' `seq` return ()) -- done
    takeOp' m0 (Just (op:ops)) = (Map.insert k ops m0, op >> keyProcLoop rf k) -- continue

-- clear pending tasks for a particular key so the thread stops quickly
clearQueue :: (Ord k) => IORef (Map k [op]) -> k -> IO ()
clearQueue rf k = atomicModifyIORef rf clearK where
    clearK m0 = (Map.alter drain k m0,())
    drain Nothing = Nothing
    drain (Just _) = Just [] 

-- clear pending tasks for all keys so all threads stop quickly
clearAllQueues :: (Ord k) => IORef (Map k [op]) -> IO ()
clearAllQueues rf = atomicModifyIORef rf clearAll where
    clearAll m0 = (Map.map (const []) m0, ())
     
-- | Watch contents of a directory with the given action. This action
-- is performed by the native implementation and is not protected.
-- Actions should be short-running, mt-safe, and not sensitive to 
-- races on one file or in one directory.
watchDir' :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO ()
watchDir' (WatchManager wc wm _) dir pr action = do
    dir' <- canonicalizeDirPath dir
    dbAction <- wrapDebounce (debounce wc) action
    let predDebounceAction ev = when (pr ev) (dbAction ev) 
    startWatch wm dir' predDebounceAction

-- | Watch contents of a directory and recursively its subdirectories
-- with the given action-constructor. The action constructor is run 
-- for each new directory. The resulting action is run for events in
-- said directory by the native implementation, and is not protected.
-- The action should be short-running, mt-safe, and not sensitive to
-- races on one file or in one directory.
watchTreeCtor' :: WatchManager -> FilePath -> ActionPredicate -> MkAction -> IO ()
watchTreeCtor' (WatchManager wc wm _) dir pr mkAction = start where
    start = canonicalizeDirPath dir >>= watch
    watch subDir = do
        action <- mkAction subDir 
        actionDebounce <- wrapDebounce (debounce wc) action
        startWatch wm subDir (wrapRecurse actionDebounce)
        children <- findDirs False subDir
        mapM_ watch children
    wrapRecurse action ev = do
        maybeNewSubdir ev
        when (pr ev) (action ev)
    maybeNewSubdir ev = 
        when (isDirEvent ev && isAddEvent ev) $ do
            childDir <- canonicalizeDirPath (eventPath ev)
            watch childDir


