--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (when, unless)
import Data.IORef (atomicModifyIORef', readIORef, newIORef, IORef)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Path (findDirs, fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified System.INotify as INo

type NativeManager = INo.INotify

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

void :: IO ()
void = return ()

-- Note that INo.Closed in this context is "modified" because we listen to
-- CloseWrite events.
fsnEvent :: FilePath -> UTCTime -> INo.Event -> Maybe Event
fsnEvent basePath timestamp (INo.Created  False       name   ) = Just (Added    (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.Closed   False (Just name) _) = Just (Modified (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.MovedOut False       name  _) = Just (Removed  (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.MovedIn  False       name  _) = Just (Added    (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.Deleted  False       name   ) = Just (Removed  (basePath </> (fp name)) timestamp)
fsnEvent _        _         _                                  = Nothing

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> INo.Event -> IO ()
-- handleInoEvent _       _    basePath _   inoEvent | trace ("Linux: handleInoEvent " ++ show basePath ++ " " ++ show inoEvent) False = undefined
handleInoEvent actPred chan basePath dbp inoEvent = do
  currentTime <- getCurrentTime
  let maybeFsnEvent = fsnEvent basePath currentTime inoEvent
  handleEvent actPred chan dbp maybeFsnEvent

handleEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> Maybe Event -> IO ()
-- handleEvent actPred _    _   (Just event) | trace ("Linux: handleEvent " ++ show (actPred event) ++ " " ++ show event) False = undefined
handleEvent actPred chan dbp (Just event) =
  when (actPred event) $ case dbp of
    (Just (DebounceData epsilon ior)) -> do
      lastEvent <- readIORef ior
      unless (debounce epsilon lastEvent event) writeToChan
      atomicModifyIORef' ior (const (event, ()))
    Nothing                           -> writeToChan
  where
    writeToChan = writeChan chan event
-- handleEvent _ _ _ Nothing | trace ("Linux handleEvent Nothing") False = undefined
handleEvent _ _ _ Nothing = void

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

instance FileListener INo.INotify where
  initSession = fmap Just INo.initINotify

  killSession = INo.killINotify

  listen db iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    wd <- INo.addWatch iNotify varieties (encodeString path') (handler path' dbp)
    return $ INo.removeWatch wd
    where
      handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  listenRecursive db iNotify path actPred chan = do
    wdVar <- newIORef (Just [])
    listenRecursiveWatchVar wdVar db iNotify path actPred chan
    return $ stopListening wdVar
    where
      stopListening wdVar = do
          mbWds <- atomicModifyIORef' wdVar $ \mbWds -> (Nothing, mbWds)
          maybe (return ()) (mapM_ INo.removeWatch) mbWds


listenRecursiveWatchVar :: IORef (Maybe [INo.WatchDescriptor])
                           -> WatchConfig
                           -> INo.INotify
                           -> FilePath
                           -> ActionPredicate
                           -> EventChannel
                           -> IO ()
listenRecursiveWatchVar wdVar db iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    paths <- findDirs True path'

    -- wdVar stores the list of created watch descriptors. We use it to
    -- cancel the whole recursive listening task.
    --
    -- To avoid a race condition (when a new watch is added right after
    -- we've stopped listening), we replace the MVar contents with Nothing
    -- to signify that the listening task is cancelled, and no new watches
    -- should be added.


    newWatches <- mapM pathHandler (path':paths)
    -- should do something nicer
    addWatches newWatches
    return ()
  where
    addWatches newWatches = atomicModifyIORef' wdVar $ \mbWds ->
        -- Atomically add a watch and record its descriptor. Also, check
        -- if the listening task is cancelled, in which case do nothing.
        case mbWds of
          Nothing -> (mbWds, ())
          Just wds -> (Just $ newWatches ++ wds, ())

    pathHandler :: FilePath -> IO INo.WatchDescriptor
    pathHandler filePath = do
      dbp <- newDebouncePayload db
      INo.addWatch iNotify varieties (fp filePath) (handler filePath dbp)
      where
        handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
        handler baseDir _   (INo.Created True dirPath) =
          listenRecursiveWatchVar wdVar db iNotify (baseDir </> fp dirPath) actPred chan
        handler baseDir dbp event                      =
          handleInoEvent actPred chan baseDir dbp event
