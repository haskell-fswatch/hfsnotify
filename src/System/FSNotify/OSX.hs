--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module System.FSNotify.OSX
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Unique
import Data.Word
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify.Listener
import System.FSNotify.Path (canonicalizeDirPath)
import System.FSNotify.Types
import System.FilePath
import qualified System.OSX.FSEvents as FSE


data WatchData = WatchData FSE.EventStream EventChannel

type WatchMap = Map Unique WatchData
data OSXManager = OSXManager (MVar WatchMap)
type NativeManager = OSXManager

nil :: Word64
nil = 0x00

-- OS X reports the absolute (canonical) path without a trailing slash. Add
-- the trailing slash when the path refers to a directory
canonicalEventPath :: FSE.Event -> FilePath
canonicalEventPath event =
  if flags .&. dirFlag /= nil then addTrailingPathSeparator path else path
  where
    flags = FSE.eventFlags event
    dirFlag = FSE.eventFlagItemIsDir
    path = FSE.eventPath event

-- We have to be careful about interpreting the flags in a given event, because
-- "really it's an OR of all the changes made since the FSEventsListener is created"
-- See https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly
-- Thus, we try to look at whether the path exists or not to determine whether it was created, modified, etc.

-- Note that there's still some bugs possible due to event coalescing, which the docs say is a possibility:
-- for example, a file could be created and modified within a short time interval, and then we'd only emit one
-- event (the "modified" one, given the logic below)
-- See https://developer.apple.com/library/content/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html
fsnEvents :: UTCTime -> FSE.Event -> IO [Event]
fsnEvents timestamp e = do
  -- Note: we *don't* want to use the canonical event path in the existence check, because of the aforementioned crazy event coalescing.
  -- For example, suppose a directory is created and deleted, and then a file is created with the same name. This means the isDirectory flag might
  -- still be turned on, which could lead us to construct a canonical event path with a trailing slash, which would then cause the existence
  -- check to fail and make us think the file was removed.
  -- The upshot of this is that the canonical event paths in the events we emit can't really be trusted, but hey, that's what the extra flag
  -- on the event is for :(
  exists <- doesPathExist $ FSE.eventPath e

  -- Uncomment for an easy way to see flag activity when testing manually
  -- putStrLn $ show ["Event", show e, "isDirectory", show isDirectory, "isFile", show isFile, "isModified", show isModified, "isCreated", show isCreated, "path", path e, "exists", show exists]

  return $ if | exists && isModified -> [Modified (path e) timestamp isDirectory]
              | exists && isCreated -> [Added (path e) timestamp isDirectory]
              | (not exists) && hasFlag e FSE.eventFlagItemRemoved -> [Removed (path e) timestamp isDirectory]

              -- Rename stuff
              | exists && isRenamed -> [Added (path e) timestamp isDirectory]
              | (not exists) && isRenamed -> [Removed (path e) timestamp isDirectory]

              | otherwise -> []
  where
    isDirectory = if hasFlag e FSE.eventFlagItemIsDir then IsDirectory else IsFile
    isFile = hasFlag e FSE.eventFlagItemIsFile
    isCreated = hasFlag e FSE.eventFlagItemCreated
    isRenamed = hasFlag e FSE.eventFlagItemRenamed
    isModified = hasFlag e FSE.eventFlagItemModified || hasFlag e FSE.eventFlagItemInodeMetaMod
    path = canonicalEventPath
    hasFlag event flag = FSE.eventFlags event .&. flag /= 0

handleFSEEvent :: Bool -> ActionPredicate -> EventChannel -> FilePath -> FSE.Event -> IO ()
handleFSEEvent isRecursive actPred chan dirPath fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  forM_ events $ \event ->
    when (actPred event && (isRecursive || (isDirectlyInside dirPath event))) $
      writeChan chan event

-- | For non-recursive monitoring, test if an event takes place directly inside the monitored folder
isDirectlyInside :: FilePath -> Event -> Bool
isDirectlyInside dirPath event = isRelevantFileEvent || isRelevantDirEvent
  where
    isRelevantFileEvent = (eventIsDirectory event == IsFile) && (takeDirectory dirPath == (takeDirectory $ eventPath event))
    isRelevantDirEvent = (eventIsDirectory event == IsDirectory) && (takeDirectory dirPath == (takeDirectory $ takeDirectory $ eventPath event))

listenFn :: (ActionPredicate -> EventChannel -> FilePath -> FSE.Event -> IO a)
         -> WatchConfig
         -> OSXManager
         -> FilePath
         -> ActionPredicate
         -> EventChannel
         -> IO StopListening
listenFn handler conf (OSXManager mvarMap) path actPred chan = do
  path' <- canonicalizeDirPath path
  unique <- newUnique
  eventStream <- FSE.eventStreamCreate [path'] 0.0 True False True (handler actPred chan path')
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert unique (WatchData eventStream chan) watchMap)
  return $ do
    FSE.eventStreamDestroy eventStream
    modifyMVar_ mvarMap $ \watchMap -> return $ Map.delete unique watchMap

instance FileListener OSXManager where
  initSession = do
    (v1, v2, _) <- FSE.osVersion
    if not $ v1 > 10 || (v1 == 10 && v2 > 6) then return $ Left "Unsupported OS version" else
      (Right . OSXManager) <$> newMVar Map.empty

  killSession (OSXManager mvarMap) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.elems watchMap) eventStreamDestroy'
    where
      eventStreamDestroy' :: WatchData -> IO ()
      eventStreamDestroy' (WatchData eventStream _) = FSE.eventStreamDestroy eventStream

  listen = listenFn $ handleFSEEvent False
  listenRecursive = listenFn $ handleFSEEvent True

  usesPolling = const False
