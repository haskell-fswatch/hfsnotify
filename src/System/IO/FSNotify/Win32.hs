--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Win32
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.Time (UTCTime, getCurrentTime)
import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path (fp, canonicalizeDirPath)
import System.IO.FSNotify.Types
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

-- NEXT TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

fsnEvents :: UTCTime -> WNo.Event -> [Event]
fsnEvents timestamp (WNo.Created  False name)                   = [Added (fp name) timestamp]
fsnEvents timestamp (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName) timestamp, Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Renamed  False Nothing newName)        = [Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Modified False (Just name))            = [Modified (fp name) timestamp]
fsnEvents timestamp (WNo.Deleted  False name)                   = [Removed (fp name) timestamp]
fsnEvents _         _                                           = []

handleWNoEvent :: ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handleWNoEvent actPred chan inoEvent = do
  currentTime <- getCurrentTime
  mapM_ (handleEvent actPred chan) (fsnEvents currentTime inoEvent)
  return ()
handleEvent :: ActionPredicate -> EventChannel -> Event -> IO ()
handleEvent actPred chan event = when (actPred event) (writeChan chan event)

void = return ()

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    _ <- WNo.watchDirectory watchManager (fp path') False varieties (handler actPred chan)
    void

  rlisten watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    _ <- WNo.watchDirectory watchManager (fp path') True varieties (handler actPred chan)
    void

handler :: ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handler = handleWNoEvent

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
