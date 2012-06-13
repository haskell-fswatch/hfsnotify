--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.OSX
       ( initSession
       , killSession
       , listen
       , rlisten
       ) where

import Prelude hiding (FilePath)

import Control.Monad
import Data.Map (Map)
import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify
import System.IO.FSNotify.Path
import qualified Data.Map as Map
import qualified System.OSX.FSEvents as FSE

data ListenType = NonRecursive | Recursive
data WatchData = WatchData FSE.EventStream ListenType Action

type WatchMap = Map FilePath WatchData
data OSXManager = OSXManager (MVar WatchMap)

fsnEvent :: FSE.Event -> Maybe Event
fsnEvent fseEvent =
  | FSE.eventFlags .&. FSE.eventFlagItemCreated  = Just (Added (fp name))
  | FSE.eventFlags .&. FSE.eventFlagItemModified = Just (Altered (fp name))
  | FSE.eventFlags .&. FSE.eventFlagItemRenamed  = Just (Added (fp name))
  | FSE.eventFlags .&. FSE.eventFlagItemRemoved  = Just (Removed (fp name))
  | otherwise                                    = Nothing

handleFSEEvent :: ActionPredicate -> Action -> FSE.Event -> IO ()
handleFSEEvent actPred action fseEvent = handleEvent actPred action (fsnEvent fseEvent)
handleEvent :: ActionPredicate -> Action -> Maybe Event -> IO ()
handleEvent actPred action (Just event) = if actPred event then action event else return ()
handlEvent _ _ Nothing = return ()

instance FileListener OSXManager where
  initSession = do
    mvarMap <- newMVar Map.empty
    return (OSXManager mvarMap)

  killSession = do
    watchMap <- readMVar mvarMap
    flip mapM_ (Map.values watchMap) $ eventStreamDestroy

  -- TODO: This will listen recursively; more code is needed to extract
  -- the directory part of file event paths and compare it to the listen type
  listen (OSXManager mvarMap) path actPred action = do
    eventStream <- FSE.eventStreamCreate [fp path] 0.0 True False True handler
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData eventStream NonRecursive action) watchMap)
    return ()
    where
      handler :: INo.Event -> IO ()
      handler = handleFSEEvent actPred action

  rlisten (OSXManager mvarMap) path actPred action = do
    eventStream <- FSE.eventStreamCreate [fp path] 0.0 True False True handler
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData eventStream Recursive action) watchMap)
    return ()
    where
      handler :: INo.Event -> IO ()
      handler = handleFSEEvent actPred action
