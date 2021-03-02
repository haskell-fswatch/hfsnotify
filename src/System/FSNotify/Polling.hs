{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Polling
  ( createPollManager
  , PollManager(..)
  , FileListener(..)
  ) where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Prelude hiding (FilePath)
import System.Directory (doesDirectoryExist)
import System.FSNotify.Listener
import System.FSNotify.Path (findFilesAndDirs, canonicalizeDirPath)
import System.FSNotify.Types
import System.FilePath
import System.PosixCompat.Files
import System.PosixCompat.Types

data EventType = AddedEvent
               | ModifiedEvent
               | RemovedEvent

newtype WatchKey = WatchKey ThreadId deriving (Eq, Ord)
data WatchData = WatchData FilePath EventCallback
type WatchMap = Map WatchKey WatchData
data PollManager = PollManager { pollManagerWatchMap :: MVar WatchMap
                               , pollManagerInterval :: Int }

generateEvent :: UTCTime -> EventIsDirectory -> EventType -> FilePath -> Maybe Event
generateEvent timestamp isDir AddedEvent filePath = Just (Added filePath timestamp isDir)
generateEvent timestamp isDir ModifiedEvent filePath = Just (Modified filePath timestamp isDir)
generateEvent timestamp isDir RemovedEvent filePath = Just (Removed filePath timestamp isDir)

generateEvents :: UTCTime -> EventType -> [(FilePath, EventIsDirectory)] -> [Event]
generateEvents timestamp eventType = mapMaybe (\(path, isDir) -> generateEvent timestamp isDir eventType path)

-- | Do not return modified events for directories.
-- These can arise when files are created inside subdirectories, resulting in the modification time
-- of the directory being bumped. However, to increase consistency with the other FileListeners,
-- we ignore these events.
handleEvent :: EventCallback -> ActionPredicate -> Event -> IO ()
handleEvent _ _ (Modified _ _ IsDirectory) = return ()
handleEvent callback actPred event
  | actPred event = callback event
  | otherwise = return ()

pathModMap :: Bool -> FilePath -> IO (Map FilePath (UTCTime, EventIsDirectory))
pathModMap recursive path = findFilesAndDirs recursive path >>= pathModMap'
  where
    pathModMap' :: [FilePath] -> IO (Map FilePath (UTCTime, EventIsDirectory))
    pathModMap' files = (Map.fromList . catMaybes) <$> mapM pathAndInfo files

    pathAndInfo :: FilePath -> IO (Maybe (FilePath, (UTCTime, EventIsDirectory)))
    pathAndInfo p = handle (\(_ :: IOException) -> return Nothing) $ do
      modTime <- getModificationTime p
      isDir <- doesDirectoryExist p
      return $ Just (p, (modTime, if isDir then IsDirectory else IsFile))

pollPath :: Int -> Bool -> EventCallback -> FilePath -> ActionPredicate -> Map FilePath (UTCTime, EventIsDirectory) -> IO ()
pollPath interval recursive callback filePath actPred oldPathMap = do
  threadDelay interval
  maybeNewPathMap <- handle (\(_ :: IOException) -> return Nothing) (Just <$> pathModMap recursive filePath)
  case maybeNewPathMap of
    -- Something went wrong while listing directories; we'll try again on the next poll
    Nothing -> pollPath interval recursive callback filePath actPred oldPathMap

    Just newPathMap -> do
      currentTime <- getCurrentTime
      let deletedMap = Map.difference oldPathMap newPathMap
          createdMap = Map.difference newPathMap oldPathMap
          modifiedAndCreatedMap = Map.differenceWith modifiedDifference newPathMap oldPathMap
          modifiedMap = Map.difference modifiedAndCreatedMap createdMap
          generateEvents' = generateEvents currentTime

      handleEvents $ generateEvents' AddedEvent [(path, isDir) | (path, (_, isDir)) <- Map.toList createdMap]
      handleEvents $ generateEvents' ModifiedEvent [(path, isDir) | (path, (_, isDir)) <- Map.toList modifiedMap]
      handleEvents $ generateEvents' RemovedEvent [(path, isDir) | (path, (_, isDir)) <- Map.toList deletedMap]

      pollPath interval recursive callback filePath actPred newPathMap

  where
    modifiedDifference :: (UTCTime, EventIsDirectory) -> (UTCTime, EventIsDirectory) -> Maybe (UTCTime, EventIsDirectory)
    modifiedDifference (newTime, isDir1) (oldTime, isDir2)
      | oldTime /= newTime || isDir1 /= isDir2 = Just (newTime, isDir1)
      | otherwise = Nothing

    handleEvents :: [Event] -> IO ()
    handleEvents = mapM_ (handleEvent callback actPred)


-- Additional init function exported to allow startManager to unconditionally
-- create a poll manager as a fallback when other managers will not instantiate.
createPollManager :: Int -> IO PollManager
createPollManager interval  = PollManager <$> newMVar Map.empty <*> pure interval

killWatchingThread :: WatchKey -> IO ()
killWatchingThread (WatchKey threadId) = killThread threadId

killAndUnregister :: MVar WatchMap -> WatchKey -> IO ()
killAndUnregister mvarMap wk = do
  _ <- withMVar mvarMap $ \m -> do
    killWatchingThread wk
    return $ Map.delete wk m
  return ()

listen' :: Bool -> WatchConfig -> PollManager -> FilePath -> ActionPredicate -> EventCallback -> IO (IO ())
listen' isRecursive _conf (PollManager mvarMap interval) path actPred callback = do
  path' <- canonicalizeDirPath path
  pmMap <- pathModMap isRecursive path'
  threadId <- forkIO $ pollPath interval isRecursive callback path' actPred pmMap
  let wk = WatchKey threadId
  modifyMVar_ mvarMap $ return . Map.insert wk (WatchData path' callback)
  return $ killAndUnregister mvarMap wk


instance FileListener PollManager Int where
  initSession interval = Right <$> createPollManager interval

  killSession (PollManager mvarMap _) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.keys watchMap) killWatchingThread

  listen = listen' False

  listenRecursive = listen' True

getModificationTime :: FilePath -> IO UTCTime
getModificationTime p = fromEpoch . modificationTime <$> getFileStatus p

fromEpoch :: EpochTime -> UTCTime
fromEpoch = posixSecondsToUTCTime . realToFrac
