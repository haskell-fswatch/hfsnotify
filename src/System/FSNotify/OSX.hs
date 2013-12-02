--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.OSX
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath, catch)

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad hiding (void)
import Data.Bits
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
-- import Debug.Trace (trace)
import Filesystem (isFile)
import Filesystem.Path hiding (concat)
import System.FSNotify.Listener
import System.FSNotify.Path (fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified Data.Map as Map
import qualified System.OSX.FSEvents as FSE

data ListenType = NonRecursive | Recursive
data WatchData = WatchData FSE.EventStream ListenType EventChannel

-- TODO: We really should use something other than FilePath as a key to allow
-- for more than one listener per FilePath.
type WatchMap = Map FilePath WatchData
data OSXManager = OSXManager (MVar WatchMap)
type NativeManager = OSXManager

void :: IO ()
void = return ()

nil :: Word64
nil = 0x00

-- OS X reports the absolute (canonical) path without a trailing slash. Add
-- the trailing slash when the path refers to a directory
canonicalEventPath :: FSE.Event -> FilePath
canonicalEventPath event =
  if flags .&. dirFlag /= nil then path </> empty else path
  where
    flags = FSE.eventFlags event
    dirFlag = FSE.eventFlagItemIsDir
    path = fp $ FSE.eventPath event

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
                    isFile (path e) >>= \exists -> if exists   then return [Added    (path e) t] else return [Removed (path e) t]
                  else
                    return []
    path = canonicalEventPath
    hasFlag event flag = FSE.eventFlags event .&. flag /= 0

-- Separate logic is needed for non-recursive events in OSX because the
-- hfsevents package doesn't support non-recursive event reporting.

handleNonRecursiveFSEEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> FSE.Event -> IO ()
-- handleNonRecursiveFSEEvent _       _    dirPath _   fseEvent | trace ("OSX: handleNonRecursiveFSEEvent " ++ show dirPath ++ " " ++ show fseEvent) False = undefined
handleNonRecursiveFSEEvent actPred chan dirPath dbp fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> [Event] -> IO ()
-- handleNonRecursiveEvents actPred _    dirPath _   (event:_     ) | trace (   "OSX: handleNonRecursiveEvents "
--                                                                       ++ show dirPath ++ " " ++ show event
--                                                                       ++ "\n  " ++ fp (directory dirPath)
--                                                                       ++ "\n  " ++ fp (directory (eventPath event))
--                                                                       ++ "\n  " ++ show (actPred event)) False = undefined
handleNonRecursiveEvents actPred chan dirPath dbp (event:events)
  | directory dirPath == directory (eventPath event) && actPred event = do
    case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing                           -> writeChan chan event
    handleNonRecursiveEvents actPred chan dirPath dbp events
  | otherwise                                                         = handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents _ _ _ _ []                                   = void

handleFSEEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> FSE.Event -> IO ()
-- handleFSEEvent _       _    _   fseEvent | trace ("OSX: handleFSEEvent " ++ show fseEvent) False = undefined
handleFSEEvent actPred chan dbp fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleEvents actPred chan dbp events

handleEvents :: ActionPredicate -> EventChannel -> DebouncePayload -> [Event] -> IO ()
-- handleEvents actPred _    _   (event:_     ) | trace ("OSX: handleEvents " ++ show event ++ " " ++ show (actPred event)) False = undefined
handleEvents actPred chan dbp (event:events) = do
  when (actPred event) $ case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing                           -> writeChan chan event
  handleEvents actPred chan dbp events
handleEvents _ _ _ [] = void

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

  listen db (OSXManager mvarMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    eventStream <- FSE.eventStreamCreate [fp path'] 0.0 True False True (handler path' dbp)
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path' (WatchData eventStream NonRecursive chan) watchMap)
    return $ return ()
    where
      handler :: FilePath -> DebouncePayload -> FSE.Event -> IO ()
      handler = handleNonRecursiveFSEEvent actPred chan

  listenRecursive db (OSXManager mvarMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    eventStream <- FSE.eventStreamCreate [fp path'] 0.0 True False True $ handler dbp
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path' (WatchData eventStream Recursive chan) watchMap)
    return $ return ()
    where
      handler :: DebouncePayload -> FSE.Event -> IO ()
      handler = handleFSEEvent actPred chan
