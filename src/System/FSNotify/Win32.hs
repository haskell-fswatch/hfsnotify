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
import System.FSNotify.Path (canonicalizeDirPath)
import System.FSNotify.Types
import Filesystem.Path
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

-- | Apparently Win32 gives back relative paths, so we pass around the base
-- directory to turn them into absolute ones
type BaseDir = FilePath

class ConvertFilePath a b where
  fp :: a -> b
instance ConvertFilePath FilePath String where fp   = encodeString
instance ConvertFilePath String FilePath where fp   = decodeString
instance ConvertFilePath String String where fp     = id
instance ConvertFilePath FilePath FilePath where fp = id

-- NEXT TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

-- Win32-notify has (temporarily?) dropped support for Renamed events.
fsnEvent :: BaseDir -> UTCTime -> WNo.Event -> Maybe Event
fsnEvent basedir timestamp ev =
  case ev of
    WNo.Created  False name -> Just $ Added    (basedir </> fp name) timestamp
    WNo.Modified False name -> Just $ Modified (basedir </> fp name) timestamp
    WNo.Deleted  False name -> Just $ Removed  (basedir </> fp name) timestamp
    _                       -> Nothing
{-
fsnEvents timestamp (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName) timestamp, Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Renamed  False Nothing newName)        = [Added (fp newName) timestamp]
-}

handleWNoEvent :: BaseDir -> ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handleWNoEvent basedir actPred chan dbp inoEvent = do
  currentTime <- getCurrentTime
  let maybeEvent = fsnEvent basedir currentTime inoEvent
  case maybeEvent of
    Just evt -> handleEvent actPred chan dbp evt
    Nothing  -> return ()
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

  listen conf watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload $ confDebounce conf
    wid <- WNo.watchDirectory watchManager (fp path') False varieties (handleWNoEvent path' actPred chan dbp)
    return $ WNo.killWatch wid

  listenRecursive conf watchManager path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload $ confDebounce conf
    wid <- WNo.watchDirectory watchManager (fp path') True varieties (handleWNoEvent path' actPred chan dbp)
    return $ WNo.killWatch wid

  usesPolling = const False

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
