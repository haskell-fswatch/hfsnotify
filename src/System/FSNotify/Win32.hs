{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Win32
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Filesystem.Path.CurrentOS ()
import System.FSNotify.Listener (debounce, FileListener(..))
import System.FSNotify.Path (fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

-- NEXT TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

void :: IO ()
void = return ()

fsnEvents :: UTCTime -> WNo.Event -> [Event]
fsnEvents timestamp (WNo.Created  False name)                   = [Added (fp name) timestamp]
fsnEvents timestamp (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName) timestamp, Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Renamed  False Nothing newName)        = [Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Modified False (Just name))            = [Modified (fp name) timestamp]
fsnEvents timestamp (WNo.Deleted  False name)                   = [Removed (fp name) timestamp]
fsnEvents _         _                                           = []

handleWNoEvent :: IOEvent -> ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handleWNoEvent ior actPred chan inoEvent = do
  currentTime <- getCurrentTime
  mapM_ (handleEvent ior actPred chan) (fsnEvents currentTime inoEvent)
handleEvent :: IOEvent -> ActionPredicate -> EventChannel -> Event -> IO ()
handleEvent ior actPred chan event =
  when (actPred event) $ do
    lastEvent <- readIORef ior
    when (not $ debounce lastEvent event) (writeChan chan event)
    atomicModifyIORef ior \_ -> (event, ())

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    ior   <- newIORef (Added (fp "") (posixSecondsToUTCTime 0))
    _ <- WNo.watchDirectory watchManager (fp path') False varieties (handler ior actPred chan)
    void

  listenRecursive watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    ior   <- newIORef (Added (fp "") (posixSecondsToUTCTime 0))
    _ <- WNo.watchDirectory watchManager (fp path') True varieties (handler ior actPred chan)
    void

handler :: IOEvent -> ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handler = handleWNoEvent

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
