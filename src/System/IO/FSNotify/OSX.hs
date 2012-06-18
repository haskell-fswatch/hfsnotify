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
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Map (Map)
import Data.Word
import Filesystem.Path
import System.IO hiding (FilePath)
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path
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

fsnEvent :: FSE.Event -> Maybe Event
fsnEvent fseEvent
  | FSE.eventFlags fseEvent .&. FSE.eventFlagItemCreated  /= nil = Just (Added (fp $ FSE.eventPath fseEvent))
  | FSE.eventFlags fseEvent .&. FSE.eventFlagItemModified /= nil = Just (Modified (fp $ FSE.eventPath fseEvent))
  | FSE.eventFlags fseEvent .&. FSE.eventFlagItemRenamed  /= nil = Just (Added (fp $ FSE.eventPath fseEvent))
  | FSE.eventFlags fseEvent .&. FSE.eventFlagItemRemoved  /= nil = Just (Removed (fp $ FSE.eventPath fseEvent))
  | otherwise                                                    = Nothing

fsnEventPath :: Event -> FilePath
fsnEventPath (Added path)    = path
fsnEventPath (Modified path) = path
fsnEventPath (Removed path)  = path

-- Separate logic is needed for non-recursive events in OSX because the
-- hfsevents package doesn't support non-recursive event reporting.
handleNonRecursiveFSEEvent :: FilePath -> ActionPredicate -> EventChannel -> FSE.Event -> IO ()
handleNonRecursiveFSEEvent dirPath actPred action fseEvent = handleNonRecursiveEvent dirPath actPred chan (fsnEvent fseEvent)
handleNonRecursiveEvent :: FilePath -> ActionPredicate -> EventChannel -> Maybe Event -> IO ()
handleNonRecursiveEvent dirPath actPred chan (Just event)
  | directory dirPath == directory (fsnEventPath event) && actPred event = writeChan event chan
  | otherwise                                                            = return ()
handleNonRecursiveEvent _ _ _ Nothing                                    = return ()

handleFSEEvent :: ActionPredicate -> EventChannel -> FSE.Event -> IO ()
handleFSEEvent actPred chan fseEvent = handleEvent actPred chan (fsnEvent fseEvent)
handleEvent :: ActionPredicate -> EventChannel -> Maybe Event -> IO ()
handleEvent actPred chan (Just event) = if actPred event then writeChan event chan else return ()
handlEvent _ _ Nothing = return ()

instance FileListener OSXManager where
  initSession = do
    (v1, v2, _) <- FSE.osVersion
    if v1 >= 10 || (v1 == 10 && v2 > 6) then
      newMVar Map.empty >>= return . Just . OSXManager
      else
      return Nothing

  killSession (OSXManager mvarMap) = do
    watchMap <- readMVar mvarMap
    flip mapM_ (Map.elems watchMap) eventStreamDestroy'
    where
      eventStreamDestroy' :: WatchData -> IO ()
      eventStreamDestroy' (WatchData eventStream _ _) = FSE.eventStreamDestroy eventStream

  listen (OSXManager mvarMap) path actPred chan = do
    eventStream <- FSE.eventStreamCreate [fp path] 0.0 True False True handler
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData eventStream NonRecursive chan) watchMap)
    return ()
    where
      handler :: FSE.Event -> IO ()
      handler = handleNonRecursiveFSEEvent path actPred chan

  rlisten (OSXManager mvarMap) path actPred chan = do
    eventStream <- FSE.eventStreamCreate [fp path] 0.0 True False True handler
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData eventStream Recursive chan) watchMap)
    return ()
    where
      handler :: FSE.Event -> IO ()
      handler = handleFSEEvent actPred chan
