--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables, ExistentialQuantification, RankNTypes, LambdaCase, OverloadedStrings, MultiWayIf, FlexibleContexts #-}

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
       , WatchConfig(..)
       , Debounce(..)
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

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Exception.Safe as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Maybe
import Data.Text as T
import System.FSNotify.Polling
import System.FSNotify.Types
import System.FilePath

import System.FSNotify.Listener (StopListening)

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
data WatchManager
  =  forall manager . FileListener manager
  => WatchManager
       WatchConfig
       manager
       (MVar (Maybe (IO ()))) -- cleanup action, or Nothing if the manager is stopped

-- | Default configuration
--
-- * Debouncing is enabled with a time interval of 1 millisecond
--
-- * Polling is disabled
--
-- * The polling interval defaults to 1 second
defaultConfig :: WatchConfig
defaultConfig =
  WatchConfig
    { confDebounce = DebounceDefault
    , confPollInterval = 10^(6 :: Int) -- 1 second
    , confUsePolling = False
    , confThreadPerEvent = True
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
startManager :: IO WatchManager
startManager = startManagerConf defaultConfig

-- | Stop a file watch manager.
-- Stopping a watch manager will immediately stop
-- watching for files and free resources.
stopManager :: WatchManager -> IO ()
stopManager (WatchManager _ wm cleanupVar) = do
  mbCleanup <- swapMVar cleanupVar Nothing
  fromMaybe (return ()) mbCleanup
  killSession wm

-- | Like 'withManager', but configurable
withManagerConf :: WatchConfig -> (WatchManager -> IO a) -> IO a
withManagerConf conf = bracket (startManagerConf conf) stopManager

-- | Like 'startManager', but configurable
startManagerConf :: WatchConfig -> IO WatchManager
startManagerConf conf = do
# ifdef OS_Win32
  -- See https://github.com/haskell-fswatch/hfsnotify/issues/50
  unless rtsSupportsBoundThreads $ throwIO $ userError "startManagerConf must be called with -threaded on Windows"
# endif

  if | confUsePolling conf -> pollingManager
     | otherwise -> initSession >>= createManager

  where
    createManager :: Either Text NativeManager -> IO WatchManager
    createManager (Right nativeManager) = WatchManager conf nativeManager <$> cleanupVar
    createManager (Left err) = do
      putStrLn $ T.unpack $ "Error: couldn't start native file manager: " <> err
      pollingManager

    pollingManager =
      WatchManager conf <$> createPollManager <*> cleanupVar

    cleanupVar = newMVar (Just (return ()))

-- | Does this manager use polling?
isPollingManager :: WatchManager -> Bool
isPollingManager (WatchManager _ wm _) = usesPolling wm

-- | Watch the immediate contents of a directory by streaming events to a Chan.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDirChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening
watchDirChan (WatchManager db wm _) = listen db wm

-- | Watch all the contents of a directory by streaming events to a Chan.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTreeChan :: WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening
watchTreeChan (WatchManager db wm _) = listenRecursive db wm

-- | Watch the immediate contents of a directory by committing an Action for each event.
-- Watching the immediate contents of a directory will only report events
-- associated with files within the specified directory, and not files
-- within its subdirectories.
watchDir :: (MonadIO m, MonadBaseControl IO m) => WatchManager -> FilePath -> ActionPredicate -> Action m -> m StopListening
watchDir = threadChan listen

-- | Watch all the contents of a directory by committing an Action for each event.
-- Watching all the contents of a directory will report events associated with
-- files within the specified directory and its subdirectories.
watchTree :: (MonadIO m, MonadBaseControl IO m) => WatchManager -> FilePath -> ActionPredicate -> Action m -> m StopListening
watchTree = threadChan listenRecursive

threadChan
  :: (MonadIO m, MonadBaseControl IO m) =>
     (forall sessionType . FileListener sessionType =>
      WatchConfig -> sessionType -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening)
      -- (^ this is the type of listen and listenRecursive)
  ->  WatchManager -> FilePath -> ActionPredicate -> Action m -> m StopListening
threadChan listenFn (WatchManager db listener cleanupVar) path actPred action =
  modifyMVar cleanupVar $ \case
    -- check if we've been stopped
    Nothing -> return (Nothing, return ()) -- or throw an exception?
    Just cleanup -> do
      chan <- newChan
      asy <- async $ readEvents db chan action
      -- Ideally, the the asy thread should be linked to the current one
      -- (@link asy@), so that it doesn't die quietly.
      -- However, if we do that, then cancelling asy will also kill
      -- ourselves. I haven't figured out how to do this (probably we
      -- should just abandon async and use lower-level primitives). For now
      -- we don't link the thread.
      stopListener <- liftIO $ listenFn db listener path actPred chan
      let cleanThisUp = cancel asy
      return
        ( Just $ cleanup >> cleanThisUp
        , stopListener >> cleanThisUp
        )

readEvents :: (MonadIO m, MonadBaseControl IO m) => WatchConfig -> EventChannel -> Action m -> m ()
readEvents (WatchConfig {confThreadPerEvent=True}) chan action = forever $ do
  event <- liftIO $ readChan chan
  us <- liftIO myThreadId
  -- Execute the event handler in a separate thread, but throw any
  -- exceptions back to us.
  --
  -- Note that there's a possibility that we may miss some exceptions, if
  -- an event handler finishes after the listen is cancelled (and so this
  -- thread is dead). How bad is that? The alternative is to kill the
  -- handler anyway when we're cancelling.
  forkFinally (action event) $ either (E.throwTo us) (const $ return ())
readEvents (WatchConfig {confThreadPerEvent=False}) chan action =
  forever $ action =<< readChan chan
