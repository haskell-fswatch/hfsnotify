--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

{-# LANGUAGE MultiWayIf #-}

module System.FSNotify.OSX
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
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

data ListenType = NonRecursive | Recursive
data WatchData = WatchData FSE.EventStream ListenType EventChannel

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
  exists <- doesPathExist (path e)

  -- putStrLn $ "Event: " <> show e
  -- putStrLn $ "isDirectory: " <> show isDirectory
  -- putStrLn $ "isFile: " <> show isFile
  -- putStrLn $ "isModified: " <> show isModified
  -- putStrLn $ "isCreated: " <> show isCreated
  -- putStrLn $ "path: " <> show (path e)
  -- putStrLn $ "exists: " <> show exists

  return $ if | exists && isModified -> [Modified (path e) timestamp isDirectory]
              | exists && isCreated -> [Added (path e) timestamp isDirectory]
              | (not exists) && hasFlag e FSE.eventFlagItemRemoved -> [Removed (path e) timestamp isDirectory]

              -- Rename stuff
              | exists && isRenamed -> [Added (path e) timestamp isDirectory]
              | (not exists) && isRenamed -> [Removed (path e) timestamp isDirectory]

              | otherwise -> []
  where
    isDirectory = hasFlag e FSE.eventFlagItemIsDir
    isFile = hasFlag e FSE.eventFlagItemIsFile
    isCreated = hasFlag e FSE.eventFlagItemCreated
    isRenamed = hasFlag e FSE.eventFlagItemRenamed
    isModified = hasFlag e FSE.eventFlagItemModified || hasFlag e FSE.eventFlagItemInodeMetaMod
    path = canonicalEventPath
    hasFlag event flag = FSE.eventFlags event .&. flag /= 0

-- Separate logic is needed for non-recursive events in OSX because the
-- hfsevents package doesn't support non-recursive event reporting.

handleNonRecursiveFSEEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> FSE.Event -> IO ()
handleNonRecursiveFSEEvent actPred chan dirPath dbp fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleNonRecursiveEvents actPred chan dirPath dbp events

handleNonRecursiveEvents :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> [Event] -> IO ()
handleNonRecursiveEvents actPred chan dirPath dbp (event:events)
  | takeDirectory dirPath == takeDirectory (eventPath event) && actPred event = do
    case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing -> writeChan chan event

    handleNonRecursiveEvents actPred chan dirPath dbp events

  | otherwise                                                         = handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents _ _ _ _ []                                   = return ()

handleRecursiveFSEEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> FSE.Event -> IO ()
handleRecursiveFSEEvent actPred chan dbp fseEvent = do
  currentTime <- getCurrentTime
  putStrLn $ "Event: " `mappend` show fseEvent
  events <- fsnEvents currentTime fseEvent
  handleRecursiveEvents actPred chan dbp events

handleRecursiveEvents :: ActionPredicate -> EventChannel -> DebouncePayload -> [Event] -> IO ()
handleRecursiveEvents actPred chan dbp (event:events) = do
  when (actPred event) $ case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing                           -> writeChan chan event
  handleRecursiveEvents actPred chan dbp events
handleRecursiveEvents _ _ _ [] = return ()

listenFn :: (ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> FSE.Event -> IO a)
         -> WatchConfig
         -> OSXManager
         -> FilePath
         -> ActionPredicate
         -> EventChannel
         -> IO StopListening
listenFn handler conf (OSXManager mvarMap) path actPred chan = do
  path' <- canonicalizeDirPath path
  dbp <- newDebouncePayload $ confDebounce conf
  unique <- newUnique
  eventStream <- FSE.eventStreamCreate [path'] 0.0 True False True (handler actPred chan path' dbp)
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert unique (WatchData eventStream NonRecursive chan) watchMap)
  return $ do
    FSE.eventStreamDestroy eventStream
    modifyMVar_ mvarMap $ \watchMap -> return $ Map.delete unique watchMap

instance FileListener OSXManager where
  initSession = do
    (v1, v2, _) <- FSE.osVersion
    if not $ v1 > 10 || (v1 == 10 && v2 > 6) then return Nothing else
      fmap (Just . OSXManager) $ newMVar Map.empty

  killSession (OSXManager mvarMap) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.elems watchMap) eventStreamDestroy'
    where
      eventStreamDestroy' :: WatchData -> IO ()
      eventStreamDestroy' (WatchData eventStream _ _) = FSE.eventStreamDestroy eventStream

  listen = listenFn handleNonRecursiveFSEEvent

  listenRecursive = listenFn $ \actPred chan _ -> handleRecursiveFSEEvent actPred chan

  usesPolling = const False
