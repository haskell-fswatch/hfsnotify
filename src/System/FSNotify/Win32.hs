--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Win32
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent
import Control.Monad (when)
import Data.Bits
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import Prelude
import System.FSNotify.Listener
import System.FSNotify.Path (canonicalizeDirPath)
import System.FSNotify.Types
import System.FilePath
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

-- | Apparently Win32 gives back relative paths, so we pass around the base
-- directory to turn them into absolute ones
type BaseDir = FilePath

-- NEXT TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

-- Win32-notify has (temporarily?) dropped support for Renamed events.
fsnEvent :: EventIsDirectory -> BaseDir -> UTCTime -> WNo.Event -> Event
fsnEvent isDirectory basedir timestamp (WNo.Created name) = Added (normalise (basedir </> name)) timestamp isDirectory
fsnEvent isDirectory basedir timestamp (WNo.Modified name) = Modified (normalise (basedir </> name)) timestamp isDirectory
fsnEvent isDirectory basedir timestamp (WNo.Deleted name) = Removed (normalise (basedir </> name)) timestamp isDirectory

handleWNoEvent :: EventIsDirectory -> BaseDir -> ActionPredicate -> EventCallback -> WNo.Event -> IO ()
handleWNoEvent isDirectory basedir actPred callback inoEvent = do
  currentTime <- getCurrentTime
  let event = fsnEvent isDirectory basedir currentTime inoEvent
  when (actPred event) $ callback event

watchDirectory :: Bool -> WatchConfig -> WNo.WatchManager -> FilePath -> ActionPredicate -> EventCallback -> IO (IO ())
watchDirectory isRecursive conf watchManager@(WNo.WatchManager mvarMap) path actPred callback = do
  path' <- canonicalizeDirPath path

  let fileFlags = foldl (.|.) 0 [WNo.fILE_NOTIFY_CHANGE_FILE_NAME, WNo.fILE_NOTIFY_CHANGE_SIZE, WNo.fILE_NOTIFY_CHANGE_ATTRIBUTES]
  let dirFlags = foldl (.|.) 0 [WNo.fILE_NOTIFY_CHANGE_DIR_NAME]

  -- Start one watch for file events and one for directory events
  -- (There seems to be no other way to provide isDirectory information)
  wid1 <- WNo.watchDirectory watchManager path' isRecursive fileFlags (handleWNoEvent IsFile path' actPred callback)
  wid2 <- WNo.watchDirectory watchManager path' isRecursive dirFlags (handleWNoEvent IsDirectory path' actPred callback)

  -- The StopListening action should make sure to remove the watches from the manager after they're killed.
  -- Otherwise, a call to killSession would cause us to try to kill them again, resulting in an invalid handle error.
  return $ do
    WNo.killWatch wid1
    modifyMVar_ mvarMap $ \watchMap -> return (Map.delete wid1 watchMap)

    WNo.killWatch wid2
    modifyMVar_ mvarMap $ \watchMap -> return (Map.delete wid2 watchMap)

instance FileListener WNo.WatchManager () where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession _ = Right <$> WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen = watchDirectory False
  listenRecursive = watchDirectory True

  usesPolling = const False
