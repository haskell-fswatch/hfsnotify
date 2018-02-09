--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Win32
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.Bits
import Data.IORef (atomicModifyIORef, readIORef)
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
fsnEvent :: Bool -> BaseDir -> UTCTime -> WNo.Event -> Event
fsnEvent isDirectory basedir timestamp (WNo.Created name) = Added (normalise (basedir </> name)) timestamp isDirectory
fsnEvent isDirectory basedir timestamp (WNo.Modified name) = Modified (normalise (basedir </> name)) timestamp isDirectory
fsnEvent isDirectory basedir timestamp (WNo.Deleted name) = Removed (normalise (basedir </> name)) timestamp isDirectory

handleWNoEvent :: Bool -> BaseDir -> ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handleWNoEvent isDirectory basedir actPred chan dbp inoEvent = do
  currentTime <- getCurrentTime
  let event = fsnEvent isDirectory basedir currentTime inoEvent
  handleEvent actPred chan dbp event

handleEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> Event -> IO ()
handleEvent actPred chan dbp event | actPred event = do
  case dbp of
    (Just (DebounceData epsilon ior)) -> do
      lastEvent <- readIORef ior
      when (not $ debounce epsilon lastEvent event) $ writeChan chan event
      atomicModifyIORef ior (\_ -> (event, ()))
    Nothing -> writeChan chan event
handleEvent _ _ _ _ = return ()

watchDirectory :: Bool -> WatchConfig -> WNo.WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO (IO ())
watchDirectory isRecursive conf watchManager path actPred chan = do
  path' <- canonicalizeDirPath path
  dbp <- newDebouncePayload $ confDebounce conf

  let fileFlags = foldl (.|.) 0 [WNo.fILE_NOTIFY_CHANGE_FILE_NAME, WNo.fILE_NOTIFY_CHANGE_SIZE]
  let dirFlags = foldl (.|.) 0 [WNo.fILE_NOTIFY_CHANGE_DIR_NAME]

  -- Start one watch for file events and one for directory events
  -- (There seems to be no other way to provide isDirectory information)
  wid1 <- WNo.watchDirectory watchManager path' isRecursive fileFlags (handleWNoEvent False path' actPred chan dbp)
  wid2 <- WNo.watchDirectory watchManager path' isRecursive dirFlags (handleWNoEvent True path' actPred chan dbp)

  return $ WNo.killWatch wid1 >> WNo.killWatch wid2

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen = watchDirectory False
  listenRecursive = watchDirectory True

  usesPolling = const False
