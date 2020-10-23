--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent.MVar
import Control.Exception.Safe as E
import Control.Monad
import qualified Data.ByteString as BS
import Data.Monoid
import Data.String
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import qualified GHC.Foreign as F
import GHC.IO.Encoding (getFileSystemEncoding)
import Prelude hiding (FilePath)
import System.FSNotify.Find
import System.FSNotify.Listener
import System.FSNotify.Path (findDirs, canonicalizeDirPath)
import System.FSNotify.Types
import System.FilePath
import qualified System.INotify as INo
import System.Posix.Files (getFileStatus, isDirectory, modificationTimeHiRes)

data INotifyListener = INotifyListener { listenerINotify :: INo.INotify }

type NativeManager = INotifyListener

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

#if MIN_VERSION_hinotify(0, 3, 10)
toRawFilePath :: FilePath -> IO BS.ByteString
toRawFilePath fp = do
  enc <- getFileSystemEncoding
  F.withCString enc fp BS.packCString

fromRawFilePath :: BS.ByteString -> IO FilePath
fromRawFilePath bs = do
  enc <- getFileSystemEncoding
  BS.useAsCString bs (F.peekCString enc)
#else
toRawFilePath = return . id
fromRawFilePath = return . id
#endif

fsnEvents :: FilePath -> UTCTime -> INo.Event -> IO [Event]
fsnEvents basePath timestamp (INo.Attributes (boolToIsDirectory -> isDir) (Just raw)) = fromRawFilePath raw >>= \name -> return [ModifiedAttributes (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Modified (boolToIsDirectory -> isDir) (Just raw)) = fromRawFilePath raw >>= \name -> return [Modified (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Created (boolToIsDirectory -> isDir) raw) = fromRawFilePath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.MovedOut (boolToIsDirectory -> isDir) raw _cookie) = fromRawFilePath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.MovedIn (boolToIsDirectory -> isDir) raw _cookie) = fromRawFilePath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Deleted (boolToIsDirectory -> isDir) raw) = fromRawFilePath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp INo.DeletedSelf = return [WatchedDirectoryRemoved basePath timestamp IsDirectory]
fsnEvents _ _ INo.Ignored = return []
fsnEvents basePath timestamp inoEvent = return [Unknown basePath timestamp IsFile (show inoEvent)]

boolToIsDirectory :: Bool -> EventIsDirectory
boolToIsDirectory False = IsFile
boolToIsDirectory True = IsDirectory

handleInoEvent :: ActionPredicate -> EventCallback -> FilePath -> MVar Bool -> INo.Event -> IO ()
handleInoEvent actPred callback basePath watchStillExistsVar inoEvent = do
  when (INo.DeletedSelf == inoEvent) $ modifyMVar_ watchStillExistsVar $ const $ return False

  currentTime <- getCurrentTime
  events <- fsnEvents basePath currentTime inoEvent
  forM_ events $ \event -> when (actPred event) $ callback event

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.Attrib, INo.Modify, INo.DeleteSelf]

instance FileListener INotifyListener () where
  initSession _ = E.handle (\(e :: IOException) -> return $ Left $ fromString $ show e) $ do
    inotify <- INo.initINotify
    return $ Right $ INotifyListener inotify

  killSession (INotifyListener {listenerINotify}) = INo.killINotify listenerINotify

  listen _conf (INotifyListener {listenerINotify}) path actPred callback = do
    path' <- canonicalizeDirPath path
    rawPath <- toRawFilePath path'
    watchStillExistsVar <- newMVar True
    wd <- INo.addWatch listenerINotify varieties rawPath (handleInoEvent actPred callback path' watchStillExistsVar)
    return $ do
      watchStillExists <- readMVar watchStillExistsVar
      when watchStillExists $ INo.removeWatch wd

  listenRecursive _conf listener initialPath actPred callback = do
    -- wdVar stores the list of created watch descriptors. We use it to
    -- cancel the whole recursive listening task.
    --
    -- To avoid a race condition (when a new watch is added right after
    -- we've stopped listening), we replace the MVar contents with Nothing
    -- to signify that the listening task is cancelled, and no new watches
    -- should be added.
    wdVar <- newMVar (Just [])

    let
      removeWatches wds = forM_ wds $ \(wd, watchStillExistsVar) ->
        handle (\(e :: SomeException) -> putStrLn ("Error removing watch: " <> show wd <> " (" <> show e <> ")"))
        (readMVar watchStillExistsVar >>= flip when (INo.removeWatch wd))

      stopListening = modifyMVar_ wdVar $ \x -> maybe (return ()) removeWatches x >> return Nothing

    path' <- canonicalizeDirPath initialPath
    paths <- findDirs True path'

    -- Add watches to this directory plus every sub-directory
    watchDirectoryRecursively listener wdVar actPred callback True path'
    forM_ paths $ watchDirectoryRecursively listener wdVar actPred callback False

    return stopListening

  usesPolling = const False


type RecursiveWatches = MVar (Maybe [(INo.WatchDescriptor, MVar Bool)])

watchDirectoryRecursively :: INotifyListener -> RecursiveWatches -> ActionPredicate -> EventCallback -> Bool -> FilePath -> IO ()
watchDirectoryRecursively listener@(INotifyListener {listenerINotify}) wdVar actPred callback isRootWatchedDir filePath = do
  rawFilePath <- toRawFilePath filePath
  modifyMVar_ wdVar $ \case
    Nothing -> return Nothing
    Just wds -> do
      watchStillExistsVar <- newMVar True
      wd <- INo.addWatch listenerINotify varieties rawFilePath (handleRecursiveEvent filePath actPred callback watchStillExistsVar isRootWatchedDir listener wdVar)
      return $ Just ((wd, watchStillExistsVar):wds)


handleRecursiveEvent :: FilePath -> ActionPredicate -> EventCallback -> MVar Bool -> Bool -> INotifyListener -> RecursiveWatches -> INo.Event -> IO ()
handleRecursiveEvent baseDir actPred callback watchStillExistsVar isRootWatchedDir listener wdVar event = do
  case event of
    (INo.Created True rawDirPath) -> do
      -- A new directory was created, so add recursive inotify watches to it
      dirPath <- fromRawFilePath rawDirPath
      let newDir = baseDir </> dirPath
      timestampBeforeAddingWatch <- getPOSIXTime
      watchDirectoryRecursively listener wdVar actPred callback False newDir

      -- Find all files/folders that might have been created *after* the timestamp, and hence might have been
      -- missed by the watch
      -- TODO: there's a chance of this generating double events, fix
      files <- find False newDir -- TODO: expose the ability to set followSymlinks to True?
      forM_ files $ \newPath -> do
        fileStatus <- getFileStatus newPath
        let modTime = modificationTimeHiRes fileStatus
        when (modTime > timestampBeforeAddingWatch) $ do
          let isDir = if isDirectory fileStatus then IsDirectory else IsFile
          let addedEvent = (Added (newDir </> newPath) (posixSecondsToUTCTime timestampBeforeAddingWatch) isDir)
          when (actPred addedEvent) $ callback addedEvent

    _ -> return ()

  -- If the watched directory was removed, mark the watch as already removed
  case event of
    INo.DeletedSelf -> modifyMVar_ watchStillExistsVar $ const $ return False
    _ -> return ()

  -- Forward the event. Ignore a DeletedSelf if we're not on the root directory,
  -- since the watch above us will pick up the delete of that directory.
  case event of
    INo.DeletedSelf | not isRootWatchedDir -> return ()
    _ -> handleInoEvent actPred callback baseDir watchStillExistsVar event
