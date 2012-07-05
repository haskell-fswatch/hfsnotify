--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Polling
  ( createPollManager
  , PollManager(..)
  , FileListener(..)
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Data.Map (Map)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Filesystem
import Filesystem.Path
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path
import System.IO.FSNotify.Types
import qualified Data.Map as Map
import Control.Monad (forM_)

data EventType =
    AddedEvent
  | ModifiedEvent
  | RemovedEvent

data WatchKey = WatchKey ThreadId deriving (Eq, Ord)
data WatchData = WatchData FilePath EventChannel
type WatchMap = Map WatchKey WatchData
data PollManager = PollManager (MVar WatchMap)

generateEvent :: EventType -> FilePath -> Maybe Event
generateEvent AddedEvent    filePath = Just (Added    filePath)
generateEvent ModifiedEvent filePath = Just (Modified filePath)
generateEvent RemovedEvent  filePath = Just (Removed  filePath)

generateEvents :: EventType -> [FilePath] -> [Event]
generateEvents eventType = mapMaybe (generateEvent eventType)

handleEvent :: EventChannel -> ActionPredicate -> Event -> IO ()
handleEvent chan actPred event
  | actPred event = writeChan chan event
  | otherwise     = return ()

pathModMap :: Bool -> FilePath -> IO (Map FilePath UTCTime)
pathModMap True  path = findFiles True path >>= pathModMap'
pathModMap False path = findFiles False path >>= pathModMap'

pathModMap' :: [FilePath] -> IO (Map FilePath UTCTime)
pathModMap' files = fmap Map.fromList $ mapM pathAndTime files
  where
    pathAndTime :: FilePath -> IO (FilePath, UTCTime)
    pathAndTime path = do
      modTime <- getModified path
      return (path, modTime)

pollPath :: Bool -> EventChannel -> FilePath -> ActionPredicate -> Map FilePath UTCTime -> IO ()
pollPath recursive chan filePath actPred oldPathMap = do
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
      | oldTime /= newTime = Just newTime
      | otherwise            = Nothing

    handleEvents :: [Event] -> IO ()
    handleEvents = mapM_ (handleEvent chan actPred)

    pollPath' :: Map FilePath UTCTime -> IO ()
    pollPath' = pollPath recursive chan filePath actPred


-- Additional init funciton exported to allow startManager to unconditionally
-- create a poll manager as a fallback when other managers will not instantiate.
createPollManager :: IO PollManager
createPollManager = fmap PollManager $ newMVar Map.empty

instance FileListener PollManager where
  initSession = fmap Just createPollManager

  killSession (PollManager mvarMap) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.keys watchMap) killThread'
    where
      killThread' :: WatchKey -> IO ()
      killThread' (WatchKey threadId) = killThread threadId

  listen (PollManager mvarMap) path actPred chan  = do
    pmMap <- pathModMap False path
    threadId <- forkIO $ pollPath False chan path actPred pmMap
    modifyMVar_ mvarMap $ return . Map.insert (WatchKey threadId) (WatchData path chan)

  rlisten (PollManager mvarMap) path actPred chan = do
    pmMap <- pathModMap True  path
    threadId <- forkIO $ pollPath True chan path actPred pmMap
    modifyMVar_ mvarMap $ return . Map.insert (WatchKey threadId) (WatchData path chan)
