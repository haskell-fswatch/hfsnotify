--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Path (findDirs, fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified System.INotify as INo


type WatchMap = Map.Map FilePath [INo.WatchDescriptor]
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


instance FileListener NativeManager where
  type WatchID NativeManager = FilePath
  initSession = do
    iNotify <- INo.initINotify
    watchMap <- newMVar Map.empty
    return $ Just $ NativeManager iNotify watchMap

  killSession (NativeManager iNotify _) = INo.killINotify iNotify

  killListener (NativeManager _ watchMap) watchDescriptor = kill where
    kill = do
        wds <- modifyMVar watchMap takeWDs 
        mapM_ INo.removeWatch wds
    takeWDs m = 
        let k = watchDescriptor in
        case Map.lookup k m of
            Nothing -> return (m,[])
            Just xs -> return (Map.delete k m, xs)

  listen db (NativeManager iNotify watchMap) path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload db
    wd <- INo.addWatch iNotify varieties (encodeString path') (handler path' dbp)
    addWatchDescriptors path' [wd] watchMap        
    return path'
    where
      handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  listenRecursive db nm path actPred chan = do
    path' <- canonicalizeDirPath path
    listenRecursive' path' db nm path' actPred chan
    return path'

addWatchDescriptors :: FilePath -> [INo.WatchDescriptor] -> MVar WatchMap -> IO ()
addWatchDescriptors k newWDs watchMap = 
    modifyMVar watchMap $ \ wm ->
        let oldWDs = fromMaybe [] (Map.lookup k wm) in
        let allWDs = nub (newWDs ++ oldWDs) in
        let wm' = Map.insert k allWDs wm in
        return (wm',())

listenRecursive' :: FilePath -> WatchConfig -> NativeManager -> FilePath 
                 -> ActionPredicate -> EventChannel -> IO ()
listenRecursive' key db nm@(NativeManager iNotify watchMap) path actPred chan = body where
    body = do
        paths <- findDirs True path
        wds <- mapM pathHandler (path:paths)
        addWatchDescriptors key wds watchMap
    pathHandler :: FilePath -> IO INo.WatchDescriptor
    pathHandler filePath = do
        dbp <- newDebouncePayload db
        INo.addWatch iNotify varieties (fp filePath) (handler filePath dbp)
    handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
    handler baseDir _   (INo.Created True dirPath) =
      listenRecursive' key db nm (baseDir </> (fp dirPath)) actPred chan
    handler baseDir dbp event                      =
      handleInoEvent actPred chan baseDir dbp event
