--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Polling
  (
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Map (Map)
import Data.Maybe
import Filesystem.Path.CurrentOS
import System.FilePath.Find
import System.IO hiding (FilePath)
import System.IO.FSNotify
import System.Posix.Types
import System.Posix.Unistd
import qualified Data.Map as Map

data EventType =
    AddedEvent
  | ModifiedEvent
  | RemovedEvent

-- Helper functions for dealing with String vs. FileSystem.Path.CurrentOS.FilePath
-- TODO: These should get pushed to the parent module
fp :: String -> FilePath
fp = decodeString
str :: FilePath -> String
str = encodeString

generateEvent :: EventType -> FilePath -> Maybe Event
generateEvent AddedEvent    filePath -> Just (Added    filePath)
generateEvent ModifiedEvent filePath -> Just (Modified filePath)
generateEvent RemovedEvent  filePath -> Just (Removed  filePath)
generateEvent _ _                    -> Nothing

generateEvents :: EventType -> [FilePath] -> [Event]
generateEvents eventType paths = mapMaybe (generateEvent eventType) paths

handleEvent :: ActionPredicate -> Action -> Event -> IO ()
handleEvent actPred action event
  | actPred event = action event
  | otherwise     = return ()

pathModMap :: FilePath -> FindClause a -> IO (Map FilePath EpochTime)
pathModMap path findClause = liftM fromList =<< fold findClause pathAndInfoCons [] path
  where
    pathAndInfoCons :: [(FilePath, EpochTime)] -> FileInfo -> [(FilePath, EpochTime)]
    pathAndInfoCons lst fileInfo = (infoPath fileInfo, modificationTime . infoStatus fileInfo):lst

pollPath :: FilePath -> FindClause -> ActionPredicate -> Action -> Map FilePath EpochTime -> IO ()
pollPath filePath findClause actPred action oldPathMap = do
  newPathMap  <- pathModMap filePath findClause
  let deletedMap = difference oldPathMap newPathMap
      createdMap = difference newPathMap oldPathMap
      modifiedAndCreatedMap = differenceWith modifiedDifference newPathMap oldPathMap
      modifiedMap = difference modifiedAndCreatedMap createdMap
  handleEvents $ generateEvents AddedEvent    $ keys createdMap
  handleEvents $ generateEvents ModifiedEvent $ keys modifiedMap
  handleEvents $ generateEvents RemovedEvent  $ keys deletedMap
  threadDelay 1000000
  pollPath' newPathMap
  where
    modifiedDifference :: EpochTime -> EpochTime -> Maybe EpochTime
    modifiedDifference newTime oldTime
      | (oldTime == newTime) = Just newTime
      | otherwise            = Nothing
    handleEvents :: [Event] -> IO [()]
    handleEvents = mapM_ (handleEvent actPred action)
    pollPath' :: Map FilePath EpochTime -> IO ()
    pollPath' = pollPath filePath actPred action

instance ListenerSession () where
  initSession   = IO ()
  killSession _ = IO ()

instance FileListener () ThreadId where
  listen nil path actPred action = forkIO $ pollPath path (depth >? 1) actPred action
  rlisten nil path actPred action = forkIO $ pollPath path always actPred action
