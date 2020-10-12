--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables, ExistentialQuantification, RankNTypes, LambdaCase, OverloadedStrings, MultiWayIf, FlexibleContexts, RecordWildCards #-}

-- | NOTE: This library does not currently report changes made to directories,
-- only files within watched directories.
--
-- Minimal example:
--
-- >{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals
-- >
-- >import System.FSNotify
-- >import Control.Concurrent (threadDelay)
-- >import Control.Monad (forever)
-- >
-- >main =
-- >  withManager $ \mgr -> do
-- >    -- start a watching job (in the background)
-- >    watchDir
-- >      mgr          -- manager
-- >      "."          -- directory to watch
-- >      (const True) -- predicate
-- >      print        -- action
-- >
-- >    -- sleep forever (until interrupted)
-- >    forever $ threadDelay 1000000

module System.FSNotify
       (

       -- * Events
         Event(..)
       , EventIsDirectory(..)
       , EventChannel
       , Action
       , ActionPredicate

       -- * Starting/Stopping
       , WatchManager
       , withManager
       , startManager
       , stopManager
       , defaultConfig
       , defaultPollingConfig
       , WatchConfig(..)
       , Debounce(..)
       , WatchMode(..)
       , ThreadingMode(..)
       , withManagerConf
       , startManagerConf
       , StopListening
       , isPollingManager

       -- * Watching
       , watchDir
       , watchDirChan
       , watchTree
       , watchTreeChan
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe as E
import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T
import System.FSNotify.Polling
import System.FSNotify.Types
import System.FilePath

import System.FSNotify.Listener (ListenFn, StopListening)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

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

-- | Watch manager. You need one in order to create watching jobs.
data WatchManager = forall manager argType. FileListener manager argType =>
  WatchManager { watchManagerConfig :: WatchConfig
               , watchManagerManager :: manager
               , watchManagerCleanupVar :: (MVar (Maybe (IO ()))) -- cleanup action, or Nothing if the manager is stopped
               , watchManagerGlobalChan :: Maybe (EventAndActionChannel, Async ())
               }

-- | Default configuration
--
-- * Uses OS watch mode and single thread.
defaultConfig :: WatchConfig
defaultConfig =
  WatchConfig
    { confDebounce = DebounceDefault
    , confWatchMode = WatchModeOS
    , confThreadingMode = SingleThread
    }

-- | Default polling configuration
--
-- * Uses polling interval of 1 second and single thread.
defaultPollingConfig :: WatchConfig
defaultPollingConfig =
  WatchConfig
    { confDebounce = DebounceDefault
    , confWatchMode = WatchModePoll (10^(6 :: Int)) -- 1 second
    , confThreadingMode = SingleThread
    }

-- | Perform an IO action with a WatchManager in place.
-- Tear down the WatchManager after the action is complete.
withManager :: (WatchManager -> IO a) -> IO a
withManager  = withManagerConf defaultConfig

-- | Start a file watch manager.
-- Directories can only be watched when they are managed by a started
-- watch manager.
-- When finished watching. you must release resources via 'stopManager'.
-- It is preferrable if possible to use 'withManager' to handle this
-- automatically.
startManager :: IO (WatchManager)
startManager = startManagerConf defaultConfig

-- | Stop a file watch manager.
-- Stopping a watch manager will immediately stop
-- watching for files and free resources.
stopManager :: WatchManager -> IO ()
stopManager (WatchManager {..}) = do
  mbCleanup <- swapMVar watchManagerCleanupVar Nothing
  maybe (return ()) liftIO mbCleanup
  liftIO $ killSession watchManagerManager
  case watchManagerGlobalChan of
    Nothing -> return ()
    Just (_, t) -> cancel t

-- | Like 'withManager', but configurable
withManagerConf :: WatchConfig -> (WatchManager -> IO a) -> IO a
withManagerConf conf = bracket (startManagerConf conf) stopManager

-- | Like 'startManager', but configurable
startManagerConf :: WatchConfig -> IO (WatchManager)
startManagerConf conf = do
# ifdef OS_Win32
  -- See https://github.com/haskell-fswatch/hfsnotify/issues/50
  unless rtsSupportsBoundThreads $ throwIO $ userError "startManagerConf must be called with -threaded on Windows"
# endif

  case confWatchMode conf of
    WatchModePoll interval -> WatchManager conf <$> liftIO (createPollManager interval) <*> cleanupVar <*> globalWatchChan
    WatchModeOS -> liftIO (initSession ()) >>= createManager

  where
    createManager :: Either Text NativeManager -> IO (WatchManager)
    createManager (Right nativeManager) = WatchManager conf nativeManager <$> cleanupVar <*> globalWatchChan
    createManager (Left err) = throwIO $ userError $ T.unpack $ "Error: couldn't start native file manager: " <> err

    globalWatchChan = case confThreadingMode conf of
      SingleThread -> do
        globalChan <- newChan
        globalReaderThread <- async $ forever $ do
          (event, action) <- readChan globalChan
          tryAny (action event) >>= \case
            Left _ -> return () -- TODO: surface the exception somehow?
            Right () -> return ()
        return $ Just (globalChan, globalReaderThread)
      _ -> return Nothing

    cleanupVar = newMVar (Just (return ()))

-- | Does this manager use polling?
isPollingManager :: WatchManager -> Bool
isPollingManager (WatchManager {..}) = usesPolling watchManagerManager

-- | Watch the immediate contents of a directory by streaming events to a Chan.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDirChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening
watchDirChan (WatchManager {..}) path actionPredicate chan = listen watchManagerConfig watchManagerManager path actionPredicate (writeChan chan)

-- | Watch all the contents of a directory by streaming events to a Chan.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTreeChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening
watchTreeChan (WatchManager {..}) path actionPredicate chan = listenRecursive watchManagerConfig watchManagerManager path actionPredicate (writeChan chan)

-- | Watch the immediate contents of a directory by committing an Action for each event.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDir :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening
watchDir = threadChan listen

-- | Watch all the contents of a directory by committing an Action for each event.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTree :: WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening
watchTree = threadChan listenRecursive

-- * Main threading logic

threadChan :: (forall a b. ListenFn a b) -> WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening
threadChan listenFn (WatchManager {watchManagerGlobalChan=(Just (globalChan, _)), ..}) path actPred action =
  modifyMVar watchManagerCleanupVar $ \case
    Nothing -> return (Nothing, return ()) -- we've been stopped. Throw an exception?
    Just cleanup -> do
      stopListener <- liftIO $ listenFn watchManagerConfig watchManagerManager path actPred (\event -> writeChan globalChan (event, action))
      return (Just cleanup, stopListener)
threadChan listenFn (WatchManager {watchManagerGlobalChan=Nothing, ..}) path actPred action =
  modifyMVar watchManagerCleanupVar $ \case
    Nothing -> return (Nothing, return ()) -- we've been stopped. Throw an exception?
    Just cleanup -> do
      chan <- newChan
      let forkThreadPerEvent = case confThreadingMode watchManagerConfig of
            SingleThread -> error "Should never happen"
            ThreadPerWatch -> False
            ThreadPerEvent -> True
      readerThread <- async $ readEvents forkThreadPerEvent chan action
      stopListener <- liftIO $ listenFn watchManagerConfig watchManagerManager path actPred (writeChan chan)
      return (Just (cleanup >> cancel readerThread), stopListener >> cancel readerThread)

readEvents :: Bool -> EventChannel -> Action -> IO ()
readEvents True chan action = forever $ do
  event <- liftIO $ readChan chan
  us <- liftIO myThreadId
  -- Execute the event handler in a separate thread, but throw any
  -- exceptions back to us.
  forkFinally (action event) $ either (E.throwTo us) (const $ return ())
readEvents False chan action =
  forever $ readChan chan >>= action
