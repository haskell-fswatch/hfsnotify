{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module System.Win32.Notify (
  Event(..)
  , EventVariety(..)
  , Handler
  , WatchId(..)
  , WatchManager(..)
  , initWatchManager
  , killWatch
  , killWatchManager
  , watch
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
import System.Win32.Types (c_maperrno_func)


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

data WatchId = WatchId [ThreadId] Handle deriving (Eq, Ord, Show)
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
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid1 <- forkIO $ dispatcher chanEvents
  tid2 <- forkIO $ osEventsReader dir watchSubTree flags watchHandle chanEvents

  -- EXPERIMENT: the OS records nothing for the handle until the reader thread reaches its first
  -- ReadDirectoryChangesW. Get off the capability so it has a chance to, before we hand back a
  -- watch the caller will use immediately.
  threadDelay 1000

  let wid = WatchId [tid1, tid2] watchHandle
  modifyMVar mvarMap $ \watchMap ->
    return (Map.insert wid handler watchMap, wid)

  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = forever $ readChan chanEvents >>= mapM_ handler

watch :: WatchManager -> FilePath -> Bool -> FileNotificationFlag -> IO (WatchId, Chan [Event])
watch (WatchManager mvarMap) dir watchSubTree flags = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid <- forkIO $ osEventsReader dir watchSubTree flags watchHandle chanEvents
  let wid = WatchId [tid] watchHandle
  modifyMVar_ mvarMap $ \watchMap ->
    return (Map.insert wid (const $ return ()) watchMap)
  return (wid, chanEvents)

osEventsReader :: FilePath -> Bool -> FileNotificationFlag -> Handle -> Chan [Event] -> IO ()
osEventsReader dir watchSubTree flags watchHandle chanEvents =
  -- EXPERIMENT (not for merge): shout if this thread ever stops, since a dead reader means the
  -- watch goes silent with no other trace
  loopBody `catch` \(e :: SomeException) -> do
    putStrLn ("WATCHDOG reader thread for " <> dir <> " flags=" <> show flags <> " died: " <> show e)
    throwIO e
  where
   loopBody = fix $ \loop ->
    readDirectoryChanges watchHandle watchSubTree flags >>= \case
     -- ERROR_OPERATION_ABORTED: this happens when the event read thread is killed.
     -- https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--500-999-
     -- Just return silently.
     Left (995, _) -> return ()
     Left (err_code, msg) -> do
       putStrLn ("WATCHDOG ReadDirectoryChangesW failed for " <> dir <> ": " <> show err_code <> " " <> msg)
       errno <- c_maperrno_func err_code
       throwIO (errnoToIOError "ReadDirectoryChangesW" errno Nothing Nothing `ioeSetErrorString` msg)
     Right events -> actsToEvents dir events >>= writeChan chanEvents >> loop

killWatch :: WatchId -> IO ()
killWatch (WatchId tids handle) = do
  forM_ tids killThread
  -- catch (closeHandle handle) $ \(e :: SomeException) ->
  --   putStrLn ([i|Failed to kill watch #{handle}: #{e}|])
  catch (closeHandle handle) $ \(_ :: SomeException) -> return ()

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
