--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Win32
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Time (getCurrentTime, UTCTime)
import System.FSNotify.Listener
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

handleWNoEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handleWNoEvent actPred chan dbp inoEvent = do
  currentTime <- getCurrentTime
  mapM_ (handleEvent actPred chan dbp) (fsnEvents currentTime inoEvent)
handleEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> Event -> IO ()
handleEvent actPred chan dbp event =
  when (actPred event) $ case dbp of
    (Just (DebounceData epsilon ior)) -> do
      lastEvent <- readIORef ior
      when (not $ debounce epsilon lastEvent event) writeToChan
      atomicModifyIORef ior (\_ -> (event, ()))
    Nothing                           -> writeToChan
  where
    writeToChan = writeChan chan event

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen db watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    _ <- WNo.watchDirectory watchManager (fp path') False varieties (handler actPred chan dbp)
    void

  listenRecursive db watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    _ <- WNo.watchDirectory watchManager (fp path') True varieties (handler actPred chan dbp)
    void

handler :: ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handler = handleWNoEvent

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
