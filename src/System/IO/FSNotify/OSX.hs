--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.OSX
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath, catch)

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
import Filesystem (isFile)
import Filesystem.Path hiding (concat)
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path (fp, canonicalizeDirPath)
import System.IO.FSNotify.Types
import qualified Data.Map as Map
import qualified System.OSX.FSEvents as FSE

data ListenType = NonRecursive | Recursive
data WatchData = WatchData FSE.EventStream ListenType EventChannel

-- TODO: We really should use something other than FilePath as a key to allow
-- for more than one listener per FilePath.
type WatchMap = Map FilePath WatchData
data OSXManager = OSXManager (MVar WatchMap)
type NativeManager = OSXManager

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
    addedFn t e = if hasFlag e FSE.eventFlagItemCreated      then return [Added    (path e) t] else return []
    modifFn t e = if hasFlag e FSE.eventFlagItemModified     then return [Modified (path e) t] else return []
    removFn t e = if hasFlag e FSE.eventFlagItemRemoved      then return [Removed  (path e) t] else return []
    renamFn t e = if hasFlag e FSE.eventFlagItemRenamed then
                    isFile (path e) >>= \exists -> if exists then return [Added    (path e) t] else return [Removed (path e) t]
                  else
                    return []
    path = canonicalEventPath
    hasFlag event flag = FSE.eventFlags event .&. flag /= 0

-- Separate logic is needed for non-recursive events in OSX because the
-- hfsevents package doesn't support non-recursive event reporting.

handleNonRecursiveFSEEvent :: FilePath -> ActionPredicate -> EventChannel -> FSE.Event -> IO ()
handleNonRecursiveFSEEvent dirPath actPred chan fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleNonRecursiveEvents dirPath actPred chan events
handleNonRecursiveEvents :: FilePath -> ActionPredicate -> EventChannel -> [Event] -> IO ()
handleNonRecursiveEvents dirPath actPred chan (event:events)
  | directory dirPath == directory (eventPath event) && actPred event = do
    writeChan chan event
    handleNonRecursiveEvents dirPath actPred chan events
  | otherwise                                                         = handleNonRecursiveEvents dirPath actPred chan events
handleNonRecursiveEvents _ _ _ []                                     = return ()

handleFSEEvent :: ActionPredicate -> EventChannel -> FSE.Event -> IO ()
handleFSEEvent actPred chan fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleEvents actPred chan events

handleEvents :: ActionPredicate -> EventChannel -> [Event] -> IO ()
handleEvents actPred chan (event:events) =
  when (actPred event) $ do
    writeChan chan event
    handleEvents actPred chan events
handleEvents _ _ [] = return ()

instance FileListener OSXManager where
  initSession = do
    (v1, v2, _) <- FSE.osVersion
    if not $ v1 >= 10 || (v1 == 10 && v2 > 6) then return Nothing else
      fmap (Just . OSXManager) $ newMVar Map.empty

  killSession (OSXManager mvarMap) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.elems watchMap) eventStreamDestroy'
    where
      eventStreamDestroy' :: WatchData -> IO ()
      eventStreamDestroy' (WatchData eventStream _ _) = FSE.eventStreamDestroy eventStream

  listen (OSXManager mvarMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    eventStream <- FSE.eventStreamCreate [fp path'] 0.0 True False True (handler path')
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path' (WatchData eventStream NonRecursive chan) watchMap)
    where
      handler :: FilePath -> FSE.Event -> IO ()
      handler listenPath = handleNonRecursiveFSEEvent listenPath actPred chan

  rlisten (OSXManager mvarMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    eventStream <- FSE.eventStreamCreate [fp path'] 0.0 True False True handler
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path' (WatchData eventStream Recursive chan) watchMap)
    where
      handler :: FSE.Event -> IO ()
      handler = handleFSEEvent actPred chan
