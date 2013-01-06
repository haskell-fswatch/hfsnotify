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
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Path (findDirs, fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified System.INotify as INo


type WatchMap = Map FilePath [INo.WatchDescriptor]
data NativeManager = NativeManager INo.INotify (MVar WatchMap)

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
      when (not $ debounce epsilon lastEvent event) writeToChan
      atomicModifyIORef ior (\_ -> (event, ()))
    Nothing                           -> writeToChan
  where
    writeToChan = writeChan chan event
-- handleEvent _ _ _ Nothing | trace ("Linux handleEvent Nothing") False = undefined
handleEvent _ _ _ Nothing = void

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

instance FileListener INo.INotify where
  type WatchID INo.INotify = FilePath
  initSession = INo.initINotify >>= \iNotify -> return $ Just $ NativeManager iNotify $ newMVar Map.empty

  killSession = INo.killINotify

  killListener (NativeManager iNotify watchMap) watchDescriptor =
    wm <- readMVar mvarMap
    case lookup watchDescriptor wm of
      Just watchDescriptors -> fmap INo.removeWatch watchDescriptors
      Nothing               -> INo.removeWatch watchDescriptor

  listen db iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    _ <- INo.addWatch iNotify varieties (encodeString path') (handler path' dbp)
    return path'
    where
      handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  listenRecursive db nm@(NativeManager iNotify watchMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    _ <- listenRecursive' path' db nm path' actPred chan
    return path'

listenRecursive' :: FilePath -> WatchConfig -> NativeManager -> FilePath -> ActionPredicate -> EventChannel -> IO INo.WatchDescriptor
listenRecursive' key db (NativeManager iNotify watchMap) path actPred chan = do
  paths <- findDirs True path
  watchDescriptors <- mapM $ pathHandler db iNotify (path:paths)
  wm <- takeMVar watchMap
  let prevWatchDescriptors = mabeToList $ lookup key
  putMVar watchMap $ insert key (watchDescriptors ++ prevWatchDescriptors)
  return key
    where
      pathHandler :: WatchConfig -> INo.INotify -> FilePath -> IO INo.WatchDesciptor
      pathHandler db iNotify filePath = do
        dbp <- newDebouncePayload db
        INo.addWatch iNotify varieties (fp filePath) (handler filePath dbp)
          where
            handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
            handler baseDir _   (INo.Created True dirPath) =
              listenRecursive' key db iNotify (baseDir </> (fp dirPath)) actPred chan
            handler baseDir dbp event                      =
              handleInoEvent actPred chan baseDir dbp event
