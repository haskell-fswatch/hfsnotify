{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Exception
import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock (UTCTime, getCurrentTime)
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
data WatchData = WatchData FilePath EventChannel
type WatchMap = Map WatchKey WatchData
newtype PollManager = PollManager (MVar WatchMap)

generateEvent :: UTCTime -> Bool -> EventType -> FilePath -> Maybe Event
generateEvent timestamp isDir AddedEvent filePath = Just (Added filePath timestamp isDir)
generateEvent timestamp isDir ModifiedEvent filePath = Just (Modified filePath timestamp isDir)
generateEvent timestamp isDir RemovedEvent filePath = Just (Removed filePath timestamp isDir)

generateEvents :: UTCTime -> EventType -> [(FilePath, Bool)] -> [Event]
generateEvents timestamp eventType = mapMaybe (\(path, isDir) -> generateEvent timestamp isDir eventType path)

-- | Do not return modified events for directories.
-- These can arise when files are created inside subdirectories, resulting in the modification time
-- of the directory being bumped. However, to increase consistency with the other FileListeners,
-- we ignore these events.
handleEvent :: EventChannel -> ActionPredicate -> Event -> IO ()
handleEvent _ _ (Modified _ _ True) = return ()
handleEvent chan actPred event
  | actPred event = writeChan chan event
  | otherwise = return ()

pathModMap :: Bool -> FilePath -> IO (Map FilePath (UTCTime, Bool))
pathModMap recursive path = findFilesAndDirs recursive path >>= pathModMap'
  where
    pathModMap' :: [FilePath] -> IO (Map FilePath (UTCTime, Bool))
    pathModMap' files = (Map.fromList . catMaybes) <$> mapM pathAndInfo files

    pathAndInfo :: FilePath -> IO (Maybe (FilePath, (UTCTime, Bool)))
    pathAndInfo path = handle (\(_ :: IOException) -> return Nothing) $ do
      modTime <- getModificationTime path
      isDir <- doesDirectoryExist path
      return $ Just (path, (modTime, isDir))

pollPath :: Int -> Bool -> EventChannel -> FilePath -> ActionPredicate -> Map FilePath (UTCTime, Bool) -> IO ()
pollPath interval recursive chan filePath actPred oldPathMap = do
  threadDelay interval
  maybeNewPathMap <- handle (\(_ :: IOException) -> return Nothing) (Just <$> pathModMap recursive filePath)
  case maybeNewPathMap of
    -- Something went wrong while listing directories; we'll try again on the next poll
    Nothing -> pollPath interval recursive chan filePath actPred oldPathMap

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

      pollPath interval recursive chan filePath actPred newPathMap

  where
    modifiedDifference :: (UTCTime, Bool) -> (UTCTime, Bool) -> Maybe (UTCTime, Bool)
    modifiedDifference (newTime, isDir1) (oldTime, isDir2)
      | oldTime /= newTime || isDir1 /= isDir2 = Just (newTime, isDir1)
      | otherwise = Nothing

    handleEvents :: [Event] -> IO ()
    handleEvents = mapM_ (handleEvent chan actPred)


-- Additional init function exported to allow startManager to unconditionally
-- create a poll manager as a fallback when other managers will not instantiate.
createPollManager :: IO PollManager
createPollManager = PollManager <$> newMVar Map.empty

killWatchingThread :: WatchKey -> IO ()
killWatchingThread (WatchKey threadId) = killThread threadId

killAndUnregister :: MVar WatchMap -> WatchKey -> IO ()
killAndUnregister mvarMap wk = do
  _ <- withMVar mvarMap $ \m -> do
    killWatchingThread wk
    return $ Map.delete wk m
  return ()

listen' :: Bool -> WatchConfig -> PollManager -> FilePath -> ActionPredicate -> EventChannel -> IO (IO ())
listen' isRecursive conf (PollManager mvarMap) path actPred chan = do
  path' <- canonicalizeDirPath path
  pmMap <- pathModMap isRecursive path'
  threadId <- forkIO $ pollPath (confPollInterval conf) isRecursive chan path' actPred pmMap
  let wk = WatchKey threadId
  modifyMVar_ mvarMap $ return . Map.insert wk (WatchData path' chan)
  return $ killAndUnregister mvarMap wk


instance FileListener PollManager where
  initSession = fmap Just createPollManager

  killSession (PollManager mvarMap) = do
    watchMap <- readMVar mvarMap
    forM_ (Map.keys watchMap) killWatchingThread

  listen = listen' False

  listenRecursive = listen' True

  usesPolling = const True


getModificationTime :: FilePath -> IO UTCTime
getModificationTime p = fromEpoch . modificationTime <$> getFileStatus p

fromEpoch :: EpochTime -> UTCTime
fromEpoch = posixSecondsToUTCTime . realToFrac
