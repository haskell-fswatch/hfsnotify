--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}

module System.IO.FSNotify.Polling
  ( initSession
  , killSession
  , listen
  , rlisten
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Filesystem.Path.CurrentOS hiding (concat)
import System.Directory
import System.FilePath hiding (FilePath, (</>))
import System.IO hiding (FilePath)
import System.IO.FSNotify
import System.Time
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
mapFP :: [String] -> [FilePath]
mapFP = map fp
mapStr :: [FilePath] -> [String]
mapStr = map str

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

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath filePath = do
  contents <- getDirectoryContents (str filePath)
  let contents' = mapFP contents
  return  $ map ((</>) filePath) $ filter (`notElem` [fp ".", fp ".."]) contents'

findImmediateFiles :: FilePath -> IO [FilePath]
findImmediateFiles filePath = do
  contents <- getDirectoryContentsPath filePath
  let contents' = mapStr contents
  files <- filterM doesFileExist contents'
  return (mapFP files)

findDirs :: FilePath -> IO [FilePath]
findDirs filePath = do
  contents <- getDirectoryContentsPath filePath
  let contents' = mapStr contents
  dirs <- filterM doesDirectoryExist contents'
  return (mapFP dirs)

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles filePath = do
  files <- findImmediateFiles filePath
  dirs  <- findDirs           filePath
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

findFiles :: Bool -> FilePath -> IO [FilePath]
findFiles True filePath = findAllFiles filePath
findFiles False filePath = findImmediateFiles filePath

pathModMap :: Bool -> FilePath -> IO (Map FilePath ClockTime)
pathModMap True path = do
  files <- findAllFiles path
  pathModMap' path files
pathModMap False path = do
  files <- findImmediateFiles path
  pathModMap' path files

pathModMap' :: FilePath -> [FilePath] -> IO (Map FilePath ClockTime)
pathModMap' path files = do
  mapList <- mapM pathAndTime files
  return (Map.fromList mapList)
  where
    pathAndTime :: FilePath -> IO (FilePath, ClockTime)
    pathAndTime path = do
      modTime <- getModificationTime (str path)
      return (path, modTime)

pollPath :: Bool -> FilePath -> ActionPredicate -> Action -> Map FilePath ClockTime -> IO ()
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
    modifiedDifference :: ClockTime -> ClockTime -> Maybe ClockTime
    modifiedDifference newTime oldTime
      | (oldTime /= newTime) = Just newTime
      | otherwise            = Nothing
    handleEvents :: [Event] -> IO ()
    handleEvents = mapM_ (handleEvent actPred action)
    pollPath' :: Map FilePath ClockTime -> IO ()
    pollPath' = pollPath recursive filePath actPred action

instance ListenerSession () where
  initSession   = return ()
  killSession _ = return ()

instance FileListener () ThreadId where
  listen nil path actPred action  = do
    pmMap <- pathModMap False path
    forkIO $ pollPath False path actPred action pmMap
  rlisten nil path actPred action = do
    pmMap <- pathModMap True  path
    tid <- forkIO $ pollPath True  path actPred action pmMap
    return [tid]
