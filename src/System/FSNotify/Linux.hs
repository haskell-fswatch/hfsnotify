--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux (
  FileListener(..)
  , NativeManager
  ) where

import Control.Concurrent.MVar
import Control.Exception.Safe as E
import Control.Monad
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.String
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Prelude hiding (FilePath)
import System.FSNotify.Find
import System.FSNotify.Linux.Util
import System.FSNotify.Listener
import System.FSNotify.Types
import System.FilePath ((</>))
import qualified System.INotify as INo
import System.Posix.ByteString (RawFilePath)
import System.Posix.Files (getFileStatus, isDirectory, modificationTimeHiRes)


data INotifyListener = INotifyListener { listenerINotify :: INo.INotify }

type NativeManager = INotifyListener

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException


fsnEvents :: RawFilePath -> UTCTime -> INo.Event -> IO [Event]
fsnEvents basePath' timestamp (INo.Attributes (boolToIsDirectory -> isDir) (Just raw)) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [ModifiedAttributes (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.Modified (boolToIsDirectory -> isDir) (Just raw)) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Modified (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.Closed (boolToIsDirectory -> isDir) (Just raw) True) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [CloseWrite (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.Created (boolToIsDirectory -> isDir) raw) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Added (basePath </> name) timestamp isDir AddedByCreate]
fsnEvents basePath' timestamp (INo.MovedOut (boolToIsDirectory -> isDir) raw _cookie) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.MovedIn (boolToIsDirectory -> isDir) raw _cookie) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Added (basePath </> name) timestamp isDir AddedByMove]
fsnEvents basePath' timestamp (INo.Deleted (boolToIsDirectory -> isDir) raw) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp INo.DeletedSelf = do
  basePath <- fromRawFilePath basePath'
  return [WatchedDirectoryRemoved basePath timestamp IsDirectory]
fsnEvents _ _ INo.Ignored = return []
fsnEvents basePath' timestamp inoEvent = do
  basePath <- fromRawFilePath basePath'
  return [Unknown basePath timestamp IsFile (show inoEvent)]

handleInoEvent :: ActionPredicate -> EventCallback -> RawFilePath -> MVar Bool -> INo.Event -> IO ()
handleInoEvent actPred callback basePath watchStillExistsVar inoEvent = do
  when (INo.DeletedSelf == inoEvent) $ modifyMVar_ watchStillExistsVar $ const $ return False

  currentTime <- getCurrentTime
  events <- fsnEvents basePath currentTime inoEvent
  forM_ events $ \event -> when (actPred event) $ callback event

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.Attrib, INo.Modify, INo.CloseWrite, INo.DeleteSelf]

instance FileListener INotifyListener () where
  initSession _ = E.handle (\(e :: IOException) -> return $ Left $ fromString $ show e) $ do
    inotify <- INo.initINotify
    return $ Right $ INotifyListener inotify

  killSession (INotifyListener {listenerINotify}) = INo.killINotify listenerINotify

  listen _conf (INotifyListener {listenerINotify}) path actPred callback = do
    rawPath <- toRawFilePath path
    canonicalRawPath <- canonicalizeRawDirPath rawPath
    watchStillExistsVar <- newMVar True
    hinotifyPath <- rawToHinotifyPath canonicalRawPath
    wd <- INo.addWatch listenerINotify varieties hinotifyPath (handleInoEvent actPred callback canonicalRawPath watchStillExistsVar)
    return $
      modifyMVar_ watchStillExistsVar $ \wse -> do
        when wse $ INo.removeWatch wd
        return False

  listenRecursive conf listener initialPath actPred callback = do
    -- wdVar stores the watch descriptors we've created, keyed by the directory they watch. We use
    -- it to cancel the whole recursive listening task.
    --
    -- To avoid a race condition (when a new watch is added right after
    -- we've stopped listening), we replace the MVar contents with Nothing
    -- to signify that the listening task is cancelled, and no new watches
    -- should be added.
    wdVar <- newMVar (Just mempty)

    let
      removeWatches wds = forM_ (Map.elems wds) $ \(wd, watchStillExistsVar) ->
        modifyMVar_ watchStillExistsVar $ \wse -> do
          when wse $ handle (reportRemoveFailure wd) (INo.removeWatch wd)
          return False

      reportRemoveFailure wd (e :: SomeException) = confOnHandlerException conf $ toException $
        userError ("error removing inotify watch " <> show wd <> ": " <> show e)

      stopListening = modifyMVar_ wdVar $ \x -> maybe (return ()) removeWatches x >> return Nothing

    -- Add watches to this directory plus every sub-directory
    rawInitialPath <- toRawFilePath initialPath
    rawCanonicalInitialPath <- canonicalizeRawDirPath rawInitialPath
    watchDirectoryRecursively listener wdVar actPred callback True rawCanonicalInitialPath
    traverseAllDirs rawCanonicalInitialPath $ \subPath ->
      watchDirectoryRecursively listener wdVar actPred callback False subPath

    return stopListening


-- | The watches making up a recursive watch. Keyed by directory so each one can drop its own
-- entry when that directory goes away.
type RecursiveWatches = MVar (Maybe (Map RawFilePath (INo.WatchDescriptor, MVar Bool)))

watchDirectoryRecursively :: INotifyListener -> RecursiveWatches -> ActionPredicate -> EventCallback -> Bool -> RawFilePath -> IO ()
watchDirectoryRecursively listener@(INotifyListener {listenerINotify}) wdVar actPred callback isRootWatchedDir rawFilePath = do
  modifyMVar_ wdVar $ \case
    Nothing -> return Nothing
    Just wds -> do
      watchStillExistsVar <- newMVar True
      hinotifyPath <- rawToHinotifyPath rawFilePath
      wd <- INo.addWatch listenerINotify varieties hinotifyPath (handleRecursiveEvent rawFilePath actPred callback watchStillExistsVar isRootWatchedDir listener wdVar)
      return $ Just (Map.insert rawFilePath (wd, watchStillExistsVar) wds)

handleRecursiveEvent :: RawFilePath -> ActionPredicate -> EventCallback -> MVar Bool -> Bool -> INotifyListener -> RecursiveWatches -> INo.Event -> IO ()
handleRecursiveEvent baseDir actPred callback watchStillExistsVar isRootWatchedDir listener wdVar event = do
  case event of
    (INo.Created True hiNotifyPath) -> do
      -- A new directory was created, so add recursive inotify watches to it
      rawDirPath <- rawFromHinotifyPath hiNotifyPath
      let newRawDir = baseDir <//> rawDirPath
      timestampBeforeAddingWatch <- getPOSIXTime
      watchDirectoryRecursively listener wdVar actPred callback False newRawDir

      newDir <- fromRawFilePath newRawDir

      -- Find all files/folders that might have been created *after* the timestamp, and hence might have been
      -- missed by the watch
      -- TODO: there's a chance of this generating double events, fix
      files <- find False newDir -- TODO: expose the ability to set followSymlinks to True?
      forM_ files $ \newPath -> do
        fileStatus <- getFileStatus newPath
        let modTime = modificationTimeHiRes fileStatus
        when (modTime > timestampBeforeAddingWatch) $ do
          let isDir = if isDirectory fileStatus then IsDirectory else IsFile
          -- We don't know whether these files were created in place or moved in, since we're
          -- synthesizing events we may have missed before the watch was in place.
          let addedEvent = (Added (newDir </> newPath) (posixSecondsToUTCTime timestampBeforeAddingWatch) isDir AddedNoExtraInfo)
          when (actPred addedEvent) $ callback addedEvent

    _ -> return ()

  -- If the watched directory was removed, mark the watch as already removed and forget it. Taking
  -- the two MVars separately, since stopListening takes them in the opposite order.
  case event of
    INo.DeletedSelf -> do
      modifyMVar_ watchStillExistsVar $ const $ return False
      modifyMVar_ wdVar $ return . fmap (Map.update dropIfStillOurs baseDir)
    _ -> return ()

  -- Forward the event. Ignore a DeletedSelf if we're not on the root directory,
  -- since the watch above us will pick up the delete of that directory.
  case event of
    INo.DeletedSelf | not isRootWatchedDir -> return ()
    _ -> handleInoEvent actPred callback baseDir watchStillExistsVar event

  where
    -- The directory may already have a new watch under the same key
    dropIfStillOurs entry@(_, var) = if var == watchStillExistsVar then Nothing else Just entry
