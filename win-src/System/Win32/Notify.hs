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
import Control.Exception.Safe (SomeException, catch, throwIO)
import Control.Monad (forM_, forever)
import Data.Function (fix)
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

-- | The dispatcher thread plus the watch it dispatches for. The reader thread isn't here on
-- purpose: it owns the buffers the OS writes into, so it has to be stopped by closing the handle
-- rather than killed (see 'killWatch').
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

  -- Put the first read in flight before returning. Otherwise changes made right after this call
  -- are lost: the OS doesn't record anything for the handle until a read is outstanding, and the
  -- reader thread below may not have been scheduled yet.
  armDirectoryWatch dirWatch >>= \case
    Right () -> return ()
    Left err -> do
      closeDirectoryWatch dirWatch
      -- No read is in flight, since arming it is what just failed
      freeDirectoryWatch dirWatch
      throwReadDirectoryChangesError err

  chanEvents <- newChan
  dispatcherTid <- forkIO $ dispatcher chanEvents
  _readerTid <- forkIO $ osEventsReader dir dirWatch chanEvents
  let wid = WatchId dispatcherTid dirWatch
  modifyMVar mvarMap $ \watchMap ->
    return (Map.insert wid handler watchMap, wid)
  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = forever $ readChan chanEvents >>= mapM_ handler

osEventsReader :: FilePath -> DirectoryWatch -> Chan [Event] -> IO ()
osEventsReader dir dirWatch chanEvents =
  -- EXPERIMENT (not for merge): shout if this thread ever stops unexpectedly
  loopBody `catch` \(e :: SomeException) -> do
    putStrLn ("WATCHDOG reader thread for " <> dir <> " died: " <> show e)
    throwIO e
  where
    loopBody = fix $ \loop ->
      awaitDirectoryWatch dirWatch >>= \case
        Right changes -> actsToEvents dir changes >>= writeChan chanEvents >> loop

        -- The watch was killed, which closed the handle and so cancelled the read. Nothing can be
        -- in flight at this point, so the buffers are ours to release.
        Left (err, _) | err == eRROR_OPERATION_ABORTED || err == eRROR_INVALID_HANDLE ->
          freeDirectoryWatch dirWatch

        Left err -> do
          freeDirectoryWatch dirWatch
          throwReadDirectoryChangesError err

killWatch :: WatchId -> IO ()
killWatch (WatchId dispatcherTid dirWatch) = do
  -- Closing the handle cancels the read in flight, which is how the reader thread learns to stop
  -- and release its buffers. Killing it outright would leave the OS writing into memory we'd have
  -- no safe moment to free.
  catch (closeDirectoryWatch dirWatch) $ \(_ :: SomeException) -> return ()
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
