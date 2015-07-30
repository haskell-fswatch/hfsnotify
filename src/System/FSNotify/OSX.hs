--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.OSX
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
import Data.Unique
import System.FilePath
import System.Directory
import System.FSNotify.Listener
import System.FSNotify.Path (canonicalizeDirPath)
import System.FSNotify.Types
import qualified Data.Map as Map
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

fsnEvents :: UTCTime -> FSE.Event -> IO [Event]
fsnEvents timestamp fseEvent = liftM concat . sequence $ map (\f -> f fseEvent) (eventFunctions timestamp)
  where
    eventFunctions :: UTCTime -> [FSE.Event -> IO [Event]]
    eventFunctions t = [addedFn t, modifFn t, removFn t, renamFn t]
    addedFn t e = if hasFlag e FSE.eventFlagItemCreated        then return [Added    (path e) t] else return []
    modifFn t e = if (hasFlag e FSE.eventFlagItemModified
                   || hasFlag e FSE.eventFlagItemInodeMetaMod) then return [Modified (path e) t] else return []
    removFn t e = if hasFlag e FSE.eventFlagItemRemoved        then return [Removed  (path e) t] else return []
    renamFn t e = if hasFlag e FSE.eventFlagItemRenamed then
                    doesFileExist (path e) >>= \exists -> if exists   then return [Added    (path e) t] else return [Removed (path e) t]
                  else
                    return []
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
      Nothing                           -> writeChan chan event
    handleNonRecursiveEvents actPred chan dirPath dbp events
  | otherwise                                                         = handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents _ _ _ _ []                                   = return ()

handleFSEEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> FSE.Event -> IO ()
handleFSEEvent actPred chan dbp fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleEvents actPred chan dbp events

handleEvents :: ActionPredicate -> EventChannel -> DebouncePayload -> [Event] -> IO ()
handleEvents actPred chan dbp (event:events) = do
  when (actPred event) $ case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing                           -> writeChan chan event
  handleEvents actPred chan dbp events
handleEvents _ _ _ [] = return ()

listenFn
  :: (ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> FSE.Event -> IO a)
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

  listenRecursive = listenFn $ \actPred chan _ -> handleFSEEvent actPred chan

  usesPolling = const False
