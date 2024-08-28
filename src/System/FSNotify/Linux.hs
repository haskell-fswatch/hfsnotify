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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux (
  FileListener(..)
  , NativeManager
  ) where

import Control.Concurrent.MVar
import Control.Exception.Safe as E
import Control.Monad
import qualified Data.ByteString as BS
import Data.Bool
import Data.Function
import Data.Monoid
import Data.String
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import qualified GHC.Foreign as F
import GHC.IO.Encoding (getFileSystemEncoding)
import Prelude hiding (FilePath)
import System.Directory (canonicalizePath)
import System.FSNotify.Find
import System.FSNotify.Listener
import System.FSNotify.Types
import System.FilePath (FilePath, (</>))
import qualified System.INotify as INo
import System.Posix.ByteString (RawFilePath)
import System.Posix.Directory.ByteString (openDirStream, readDirStream, closeDirStream)
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
  fromHinotifyPath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.MovedOut (boolToIsDirectory -> isDir) raw _cookie) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath' timestamp (INo.MovedIn (boolToIsDirectory -> isDir) raw _cookie) = do
  basePath <- fromRawFilePath basePath'
  fromHinotifyPath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
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
        modifyMVar_ watchStillExistsVar $ \wse -> do
          when wse $
            handle (\(e :: SomeException) -> putStrLn ("Error removing watch: " <> show wd <> " (" <> show e <> ")"))
                   (INo.removeWatch wd)
          return False

      stopListening = modifyMVar_ wdVar $ \x -> maybe (return ()) removeWatches x >> return Nothing

    -- Add watches to this directory plus every sub-directory
    rawInitialPath <- toRawFilePath initialPath
    rawCanonicalInitialPath <- canonicalizeRawDirPath rawInitialPath
    watchDirectoryRecursively listener wdVar actPred callback True rawCanonicalInitialPath
    traverseAllDirs rawCanonicalInitialPath (confPathFilter conf) $ \subPath ->
      watchDirectoryRecursively listener wdVar actPred callback False subPath

    return stopListening


type RecursiveWatches = MVar (Maybe [(INo.WatchDescriptor, MVar Bool)])

watchDirectoryRecursively :: INotifyListener -> RecursiveWatches -> ActionPredicate -> EventCallback -> Bool -> RawFilePath -> IO ()
watchDirectoryRecursively listener@(INotifyListener {listenerINotify}) wdVar actPred callback isRootWatchedDir rawFilePath = do
  modifyMVar_ wdVar $ \case
    Nothing -> return Nothing
    Just wds -> do
      watchStillExistsVar <- newMVar True
      hinotifyPath <- rawToHinotifyPath rawFilePath
      wd <- INo.addWatch listenerINotify varieties hinotifyPath (handleRecursiveEvent rawFilePath actPred callback watchStillExistsVar isRootWatchedDir listener wdVar)
      return $ Just ((wd, watchStillExistsVar):wds)


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



-- * Util

canonicalizeRawDirPath :: RawFilePath -> IO RawFilePath
canonicalizeRawDirPath p = fromRawFilePath p >>= canonicalizePath >>= toRawFilePath

-- | Same as </> but for RawFilePath
-- TODO: make sure this is correct or find in a library
(<//>) :: RawFilePath -> RawFilePath -> RawFilePath
x <//> y = x <> "/" <> y

traverseAllDirs :: RawFilePath -> (FilePath -> IO Bool) -> (RawFilePath -> IO ()) -> IO ()
traverseAllDirs dir predicate cb = traverseAll dir $ \subRawPath -> do
    subPath <- fromRawFilePath subRawPath
    needWatch <- predicate subPath
    if not needWatch then return False
    else do
      -- TODO: wish we didn't need fromRawFilePath here
      -- TODO: make sure this does the right thing with symlinks
      getFileStatus subPath >>= \case
        (isDirectory -> True) -> cb subRawPath >> return True
        _ -> return False

traverseAll :: RawFilePath -> (RawFilePath -> IO Bool) -> IO ()
traverseAll dir cb = bracket (openDirStream dir) closeDirStream $ \dirStream ->
  fix $ \loop -> do
    readDirStream dirStream >>= \case
      x | BS.null x -> return ()
      "." -> loop
      ".." -> loop
      subDir -> flip finally loop $ do
        -- TODO: canonicalize?
        let fullSubDir = dir <//> subDir
        shouldRecurse <- cb fullSubDir
        when shouldRecurse $ traverseAll fullSubDir cb

boolToIsDirectory :: Bool -> EventIsDirectory
boolToIsDirectory False = IsFile
boolToIsDirectory True = IsDirectory

toRawFilePath :: FilePath -> IO BS.ByteString
toRawFilePath fp = do
  enc <- getFileSystemEncoding
  F.withCString enc fp BS.packCString

fromRawFilePath :: BS.ByteString -> IO FilePath
fromRawFilePath bs = do
  enc <- getFileSystemEncoding
  BS.useAsCString bs (F.peekCString enc)

#if MIN_VERSION_hinotify(0, 3, 10)
fromHinotifyPath :: BS.ByteString -> IO FilePath
fromHinotifyPath = fromRawFilePath

rawToHinotifyPath :: BS.ByteString -> IO BS.ByteString
rawToHinotifyPath = return

rawFromHinotifyPath :: BS.ByteString -> IO BS.ByteString
rawFromHinotifyPath = return
#else
fromHinotifyPath :: FilePath -> IO FilePath
fromHinotifyPath = return

rawToHinotifyPath :: BS.ByteString -> IO FilePath
rawToHinotifyPath = fromRawFilePath

rawFromHinotifyPath :: FilePath -> IO BS.ByteString
rawFromHinotifyPath = toRawFilePath
#endif
