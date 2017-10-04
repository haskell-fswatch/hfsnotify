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

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
import Prelude hiding (FilePath)
import System.FSNotify.Listener
import System.FSNotify.Path (findDirs, canonicalizeDirPath)
import System.FSNotify.Types
import System.FilePath
import qualified System.INotify as INo

type NativeManager = INo.INotify

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

-- Note that INo.Closed in this context is "modified" because we listen to
-- CloseWrite events.
fsnEvent :: FilePath -> UTCTime -> INo.Event -> Event
fsnEvent basePath timestamp (INo.Created isDirectory name) = Added    (basePath </> name) timestamp isDirectory
fsnEvent basePath timestamp (INo.Closed isDirectory (Just name) True) = Modified (basePath </> name) timestamp isDirectory
fsnEvent basePath timestamp (INo.MovedOut isDirectory name _cookie) = Removed  (basePath </> name) timestamp isDirectory
fsnEvent basePath timestamp (INo.MovedIn isDirectory name _cookie) = Added    (basePath </> name) timestamp isDirectory
fsnEvent basePath timestamp (INo.Deleted isDirectory name ) = Removed  (basePath </> name) timestamp isDirectory
fsnEvent basePath timestamp inoEvent = Unknown basePath timestamp (show inoEvent)

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> INo.Event -> IO ()
handleInoEvent actPred chan basePath dbp inoEvent = do
  currentTime <- getCurrentTime
  let event = fsnEvent basePath currentTime inoEvent
  handleEvent actPred chan dbp event

handleEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> Event -> IO ()
handleEvent actPred chan dbp event =
  when (actPred event) $ case dbp of
    (Just (DebounceData epsilon ior)) -> do
      lastEvent <- readIORef ior
      when (not $ debounce epsilon lastEvent event) writeToChan
      atomicModifyIORef ior (\_ -> (event, ()))
    Nothing                           -> writeToChan
  where
    writeToChan = writeChan chan event

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

instance FileListener INo.INotify where
  initSession = fmap Just INo.initINotify

  killSession = INo.killINotify

  listen conf iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload $ confDebounce conf
    wd <- INo.addWatch iNotify varieties path' (handler path' dbp)
    return $ INo.removeWatch wd
    where
      handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  listenRecursive conf iNotify initialPath actPred chan = do
    -- wdVar stores the list of created watch descriptors. We use it to
    -- cancel the whole recursive listening task.
    --
    -- To avoid a race condition (when a new watch is added right after
    -- we've stopped listening), we replace the MVar contents with Nothing
    -- to signify that the listening task is cancelled, and no new watches
    -- should be added.
    wdVar <- newMVar (Just [])

    let
      stopListening = do
        modifyMVar_ wdVar $ \mbWds -> do
          maybe (return ()) (mapM_ INo.removeWatch) mbWds
          return Nothing

    listenRec initialPath wdVar

    return stopListening

    where
      listenRec :: FilePath -> MVar (Maybe [INo.WatchDescriptor]) -> IO ()
      listenRec path wdVar = do
        path' <- canonicalizeDirPath path
        paths <- findDirs True path'

        mapM_ (pathHandler wdVar) (path':paths)

      pathHandler :: MVar (Maybe [INo.WatchDescriptor]) -> FilePath -> IO ()
      pathHandler wdVar filePath = do
        dbp <- newDebouncePayload $ confDebounce conf
        modifyMVar_ wdVar $ \mbWds ->
          -- Atomically add a watch and record its descriptor. Also, check
          -- if the listening task is cancelled, in which case do nothing.
          case mbWds of
            Nothing -> return mbWds
            Just wds -> do
              wd <- INo.addWatch iNotify varieties filePath (handler filePath dbp)
              return $ Just (wd:wds)
        where
          handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
          handler baseDir dbp event = do
            -- When a new directory is created, add recursive inotify watches to it
            -- TODO: there's a race condition here; if there are files present in the directory before
            -- we add the watches, we'll miss them. The right thing to do would be to ls the directory
            -- and trigger Added events for everything we find there
            case event of
              (INo.Created True dirPath) -> listenRec (baseDir </> dirPath) wdVar
              _ -> return ()

            -- Forward all events, including directory create
            handleInoEvent actPred chan baseDir dbp event

  usesPolling = const False
