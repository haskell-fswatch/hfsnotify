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
import Control.Exception.Safe (throwIO)
import Control.Monad (forM_, forever)
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.C.Error (errnoToIOError)
import System.FilePath
import System.IO.Error (ioeSetErrorString)
import System.Win32.File
import System.Win32.FileNotify
import System.Win32.Types (c_maperrno_func, ErrCode)


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

-- | The watch, plus the thread that runs its handler. The reader thread isn't here on purpose: it
-- is stopped by cancelling its read rather than by being killed, since the OS writes into memory
-- it owns (see 'killWatch').
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

watchDirectory :: WatchManager -> FilePath -> Bool -> FileNotificationFlag -> Handler -> IO WatchId
watchDirectory (WatchManager mvarMap) dir watchSubTree flags handler = do
  dirWatch <- openDirectoryWatch dir watchSubTree flags

  chanEvents <- newChan
  armed <- newEmptyMVar

  -- The reader issues the first read and reports back once it's in flight, so we never hand out a
  -- watch that isn't listening yet: Windows records nothing for the handle until a read has been
  -- issued, and a change made before that is lost with nothing to recover it from.
  --
  -- It has to be the reader that issues it, on a bound thread, for two reasons. Windows cancels
  -- pending overlapped I/O when the thread that issued it exits, and only the reader is
  -- guaranteed to outlive the read; and a plain forkIO thread's FFI calls can land on different
  -- RTS workers, which come and go.
  _readerTid <- forkOS $ osEventsReader armed dir dirWatch chanEvents
  takeMVar armed >>= \case
    Right () -> return ()
    Left err -> do
      -- Arming is what failed, so nothing is in flight and the reader has already given up
      abandonDirectoryWatch dirWatch
      throwReadDirectoryChangesError err

  dispatcherTid <- forkIO $ dispatcher chanEvents
  let wid = WatchId dispatcherTid dirWatch
  modifyMVar mvarMap $ \watchMap ->
    return (Map.insert wid handler watchMap, wid)

  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = forever $ readChan chanEvents >>= mapM_ handler

-- | Issue the first read, report whether it's in flight, and then deliver events until the watch
-- is stopped.
--
-- Reporting back when it stops is part of the contract too: 'stopDirectoryWatch' can only release
-- the memory the OS writes into once it knows no read is in flight.
osEventsReader :: MVar (Either (ErrCode, String) ()) -> FilePath -> DirectoryWatch -> Chan [Event] -> IO ()
osEventsReader armed dir dirWatch chanEvents =
  armDirectoryWatch dirWatch >>= \case
    Left err -> putMVar armed (Left err)
    Right () -> do
      putMVar armed (Right ())
      loop
  where
    loop = awaitDirectoryWatch dirWatch >>= \case
      Right changes -> do
        actsToEvents dir changes >>= writeChan chanEvents
        directoryWatchStopping dirWatch >>= \case
          True -> signalReaderFinished dirWatch
          False -> armDirectoryWatch dirWatch >>= \case
            Right () -> loop
            Left err -> finishWith err
      Left err -> finishWith err

    -- Either the read was cancelled because the watch is being stopped, or it failed. Either way
    -- the OS is done with the buffer, so say so; only complain if this wasn't a stop.
    finishWith err = do
      signalReaderFinished dirWatch
      directoryWatchStopping dirWatch >>= \case
        True -> return ()
        False -> throwReadDirectoryChangesError err

killWatch :: WatchId -> IO ()
killWatch (WatchId dispatcherTid dirWatch) = do
  stopDirectoryWatch dirWatch
  killThread dispatcherTid

throwReadDirectoryChangesError :: (ErrCode, String) -> IO a
throwReadDirectoryChangesError (errCode, msg) = do
  errno <- c_maperrno_func errCode
  throwIO (errnoToIOError "ReadDirectoryChangesW" errno Nothing Nothing `ioeSetErrorString` msg)

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
