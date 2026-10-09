{-# LANGUAGE LambdaCase #-}

module System.Win32.Notify (
  Event(..)
  , EventVariety(..)
  , Handler
  , WatchId(..)
  , WatchManager(..)
  , initWatchManager
  , killWatch
  , killWatchManager
  , watchDirectory

  , fILE_NOTIFY_CHANGE_FILE_NAME
  , fILE_NOTIFY_CHANGE_DIR_NAME
  , fILE_NOTIFY_CHANGE_ATTRIBUTES
  , fILE_NOTIFY_CHANGE_SIZE
  , fILE_NOTIFY_CHANGE_LAST_WRITE
  -- , fILE_NOTIFY_CHANGE_LAST_ACCESS
  -- , fILE_NOTIFY_CHANGE_CREATION
  , fILE_NOTIFY_CHANGE_SECURITY
  ) where

import Control.Concurrent
import Control.Exception.Safe (IOException, SomeException, bracketOnError, toException, try)
import Control.Monad (forM_, forever)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath
import System.Win32.File
import System.Win32.FileNotify


data EventVariety =
  Modify
  | Create
  | Delete
  | Move
  deriving Eq

data Event
  -- | A file was modified. @Modified isDirectory file@
  = Modified { filePath :: FilePath }
  -- | A file was created. @Created isDirectory file@
  | Created { filePath :: FilePath }
  -- | A file was deleted. @Deleted isDirectory file@
  | Deleted { filePath :: FilePath }
  deriving (Eq, Show)

type Handler = Event -> IO ()

-- | A watch, plus the thread dispatching its events.
data WatchId = WatchId ThreadId DirectoryWatch deriving (Eq, Ord, Show)
type WatchMap = Map WatchId Handler
data WatchManager = WatchManager { watchManagerWatchMap :: MVar WatchMap }

initWatchManager :: IO WatchManager
initWatchManager = WatchManager <$> newMVar Map.empty

killWatchManager :: WatchManager -> IO ()
killWatchManager (WatchManager mvarMap) = do
  modifyMVar_ mvarMap $ \watchMap -> do
    forM_ (Map.keys watchMap) killWatch
    return mempty

watchDirectory :: WatchManager -> FilePath -> Bool -> FileNotificationFlag -> (SomeException -> IO ()) -> Handler -> IO WatchId
watchDirectory (WatchManager mvarMap) dir watchSubTree flags onWatchError handler =
  bracketOnError (openDirectoryWatch dir watchSubTree flags onWatchError) stopDirectoryWatch $ \dirWatch -> do
    chanEvents <- newChan

    -- Issues the first read and leaves its reader running, so we never hand out a watch that isn't
    -- listening yet, and throws rather than returning if the read couldn't be issued
    startDirectoryWatch dirWatch $ osEventsReader dir dirWatch chanEvents

    dispatcherTid <- forkIO $ dispatcher chanEvents
    let wid = WatchId dispatcherTid dirWatch
    modifyMVar mvarMap $ \watchMap ->
      return (Map.insert wid handler watchMap, wid)

  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = forever $ readChan chanEvents >>= mapM_ handler

osEventsReader :: FilePath -> DirectoryWatch -> Chan [Event] -> IO ()
osEventsReader dir dirWatch chanEvents = loop
  where
    loop = do
      -- Each pass is its own 'try' so the handlers don't stack up over the life of the watch
      outcome <- try $ do
        changes <- awaitDirectoryWatch dirWatch
        actsToEvents dir changes >>= writeChan chanEvents
        directoryWatchStopping dirWatch >>= \case
          True -> return False
          False -> True <$ armDirectoryWatch dirWatch

      case outcome of
        Right True -> loop
        Right False -> return ()
        -- A cancelled read means we're being stopped; anything else is a watch that has died, and
        -- will report nothing more, which the user wants to know about.
        Left (err :: IOException) -> directoryWatchStopping dirWatch >>= \case
          True -> return ()
          False -> directoryWatchOnError dirWatch (toException err)

killWatch :: WatchId -> IO ()
killWatch (WatchId dispatcherTid dirWatch) = do
  -- Stopping the watch cancels the reader's read and waits for it to finish, rather than killing
  -- it: killing it mid-read would leave us no way to know when the OS is done with its buffer.
  stopDirectoryWatch dirWatch
  killThread dispatcherTid


actsToEvents :: FilePath -> [(Action, String)] -> IO [Event]
actsToEvents baseDir = mapM actToEvent
  where
    actToEvent (act, fn) = do
      case act of
        FileModified -> return $ Modified $ baseDir </> fn
        FileAdded -> return $ Created $ baseDir </> fn
        FileRemoved -> return $ Deleted $ baseDir </> fn
        FileRenamedOld -> return $ Deleted $ baseDir </> fn
        FileRenamedNew -> return $ Created $ baseDir </> fn
