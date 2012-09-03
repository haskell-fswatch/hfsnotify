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
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (when)
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify.Listener (debounce, epsilon, FileListener(..))
import System.IO.FSNotify.Path (fp, canonicalizeDirPath)
import System.IO.FSNotify.Types
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

type MEvent = MVar Event

-- NEXT TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

void = return ()

fsnEvents :: UTCTime -> WNo.Event -> [Event]
fsnEvents timestamp (WNo.Created  False name)                   = [Added (fp name) timestamp]
fsnEvents timestamp (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName) timestamp, Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Renamed  False Nothing newName)        = [Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Modified False (Just name))            = [Modified (fp name) timestamp]
fsnEvents timestamp (WNo.Deleted  False name)                   = [Removed (fp name) timestamp]
fsnEvents _         _                                           = []

handleWNoEvent :: MEvent -> ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handleWNoEvent mvar actPred chan inoEvent = do
  currentTime <- getCurrentTime
  mapM_ (handleEvent mvar actPred chan) (fsnEvents currentTime inoEvent)
  void
handleEvent :: MEvent -> ActionPredicate -> EventChannel -> Event -> IO ()
handleEvent mvar actPred chan event =
  when (actPred event) $ do
    lastEvent <- readMVar mvar
    when (not $ debounce lastEvent event) (writeChan chan event)
    swapMVar mvar event
    void

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    mvar  <- newMVar (Added (fp "") (posixSecondsToUTCTime 0))
    _ <- WNo.watchDirectory watchManager (fp path') False varieties (handler mvar actPred chan)
    void

  rlisten watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    mvar  <- newMVar (Added (fp "") (posixSecondsToUTCTime 0))
    _ <- WNo.watchDirectory watchManager (fp path') True varieties (handler mvar actPred chan)
    void

handler :: MEvent -> ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handler = handleWNoEvent

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
