
module System.Win32.Notify
  ( Event(..)
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
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Bits
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import System.Win32 (closeHandle)
import System.Win32.File
import System.Win32.FileNotify

data EventVariety = Modify
                  | Create
                  | Delete
                  | Move deriving Eq

data Event
    -- | A file was modified. @Modified isDirectory file@
    = Modified { filePath :: FilePath }
    -- | A file was created. @Created isDirectory file@
    | Created { filePath :: FilePath }
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted { filePath :: FilePath }
    deriving (Eq, Show)

type Handler = Event -> IO ()

data WatchId = WatchId ThreadId ThreadId Handle deriving (Eq, Ord, Show)
type WatchMap = Map WatchId Handler
data WatchManager = WatchManager (MVar WatchMap)

initWatchManager :: IO WatchManager
initWatchManager =  do
  mvarMap <- newMVar Map.empty
  return (WatchManager mvarMap)

killWatchManager :: WatchManager -> IO ()
killWatchManager (WatchManager mvarMap) = do
  watchMap <- readMVar mvarMap
  flip mapM_ (Map.keys watchMap) $ killWatch

watchDirectory :: WatchManager -> FilePath -> Bool -> FileNotificationFlag -> Handler -> IO WatchId
watchDirectory (WatchManager mvarMap) dir watchSubTree flags handler = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid1 <- forkIO $ dispatcher chanEvents
  tid2 <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid1 tid2 watchHandle) handler watchMap)
  return (WatchId tid1 tid2 watchHandle)

  where
    dispatcher :: Chan [Event] -> IO ()
    dispatcher chanEvents = forever $ readChan chanEvents >>= mapM_ handler

    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = forever $ do
      (readDirectoryChanges watchHandle watchSubTree flags >>= (actsToEvents dir) >>= writeChan chanEvents)

watch :: WatchManager -> FilePath -> Bool -> FileNotificationFlag -> IO (WatchId, Chan [Event])
watch (WatchManager mvarMap) dir watchSubTree flags = do
  watchHandle <- getWatchHandle dir
  chanEvents <- newChan
  tid <- forkIO $ osEventsReader watchHandle chanEvents
  modifyMVar_ mvarMap $ \watchMap -> return (Map.insert (WatchId tid tid watchHandle) (const $ return ()) watchMap)
  return ((WatchId tid tid watchHandle), chanEvents)

  where
    osEventsReader :: Handle -> Chan [Event] -> IO ()
    osEventsReader watchHandle chanEvents = forever $ do
      (readDirectoryChanges watchHandle watchSubTree flags >>= (actsToEvents dir) >>= writeChan chanEvents)

killWatch :: WatchId -> IO ()
killWatch (WatchId tid1 tid2 handle) = do
    killThread tid1
    if tid1 /= tid2 then killThread tid2 else return ()
    closeHandle handle

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
