--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Polling
  ( PollManager
  , initSession
  , killSession
  , listen
  , rlisten
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Filesystem
import Filesystem.Path
import System.IO hiding (FilePath)
import System.IO.FSNotify
import System.IO.FSNotify.Path
import qualified Data.Map as Map

data EventType =
    AddedEvent
  | ModifiedEvent
  | RemovedEvent

data WatchData = WatchData ThreadId Action
type WatchMap = Map FilePath WatchData
data PollManager = PollManager (MVar WatchMap)

generateEvent :: EventType -> FilePath -> Maybe Event
generateEvent AddedEvent    filePath = Just (Added    filePath)
generateEvent ModifiedEvent filePath = Just (Modified filePath)
generateEvent RemovedEvent  filePath = Just (Removed  filePath)

generateEvents :: EventType -> [FilePath] -> [Event]
generateEvents eventType paths = mapMaybe (generateEvent eventType) paths

handleEvent :: ActionPredicate -> Action -> Event -> IO ()
handleEvent actPred action event
  | actPred event = action event
  | otherwise     = return ()

pathModMap :: Bool -> FilePath -> IO (Map FilePath UTCTime)
pathModMap True path = do
  files <- findFiles True path
  pathModMap' path files
pathModMap False path = do
  files <- findFiles False path
  pathModMap' path files

pathModMap' :: FilePath -> [FilePath] -> IO (Map FilePath UTCTime)
pathModMap' path files = do
  mapList <- mapM pathAndTime files
  return (Map.fromList mapList)
  where
    pathAndTime :: FilePath -> IO (FilePath, UTCTime)
    pathAndTime path = do
      modTime <- getModified path
      return (path, modTime)

pollPath :: Bool -> FilePath -> ActionPredicate -> Action -> Map FilePath UTCTime -> IO ()
pollPath recursive filePath actPred action oldPathMap = do
  threadDelay 1000000
  newPathMap  <- pathModMap recursive filePath
  let deletedMap = Map.difference oldPathMap newPathMap
      createdMap = Map.difference newPathMap oldPathMap
      modifiedAndCreatedMap = Map.differenceWith modifiedDifference newPathMap oldPathMap
      modifiedMap = Map.difference modifiedAndCreatedMap createdMap
  handleEvents $ generateEvents AddedEvent    $ Map.keys createdMap
  handleEvents $ generateEvents ModifiedEvent $ Map.keys modifiedMap
  handleEvents $ generateEvents RemovedEvent  $ Map.keys deletedMap
  pollPath' newPathMap
  where
    modifiedDifference :: UTCTime -> UTCTime -> Maybe UTCTime
    modifiedDifference newTime oldTime
      | (oldTime /= newTime) = Just newTime
      | otherwise            = Nothing
    handleEvents :: [Event] -> IO ()
    handleEvents = mapM_ (handleEvent actPred action)
    pollPath' :: Map FilePath UTCTime -> IO ()
    pollPath' = pollPath recursive filePath actPred action

instance FileListener PollManager where
  initSession = do
    mvarMap <- newMVar Map.empty
    return (PollManager mvarMap)

  killSession (PollManager mvarMap) = do
    watchMap <- readMVar mvarMap
    flip mapM_ (Map.elems watchMap) $ killThread'
    where
      killThread' :: WatchData -> IO ()
      killThread' (WatchData threadId _) = killThread threadId

  listen (PollManager mvarMap) path actPred action  = do
    pmMap <- pathModMap False path
    threadId <- forkIO $ pollPath False path actPred action pmMap
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData threadId action) watchMap)
    return ()

  rlisten (PollManager mvarMap) path actPred action = do
    pmMap <- pathModMap True  path
    threadId <- forkIO $ pollPath True  path actPred action pmMap
    modifyMVar_ mvarMap $ \watchMap -> return (Map.insert path (WatchData threadId action) watchMap)
    return ()
