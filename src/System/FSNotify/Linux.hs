--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception as E
import Control.Monad
import qualified Data.ByteString as BS
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Typeable
import qualified GHC.Foreign as F
import GHC.IO.Encoding (getFileSystemEncoding)
import Prelude hiding (FilePath)
import qualified Shelly as S
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
fsnEvents basePath timestamp (INo.Attributes (boolToIsDirectory -> isDir) (Just raw)) = fromRawFilePath raw >>= \name -> return [Modified (basePath </> name) timestamp isDir]
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

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> MVar Bool -> INo.Event -> IO ()
handleInoEvent actPred chan basePath watchStillExistsVar inoEvent = do
  when (INo.DeletedSelf == inoEvent) $ modifyMVar_ watchStillExistsVar $ const $ return False

  currentTime <- getCurrentTime
  events <- fsnEvents basePath currentTime inoEvent
  forM_ events $ \event -> when (actPred event) $ writeChan chan event

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.Attrib, INo.Modify, INo.DeleteSelf]

instance FileListener INotifyListener where
  initSession = E.handle (\(e :: IOException) -> return $ Left $ fromString $ show e) $ do
    inotify <- INo.initINotify
    return $ Right $ INotifyListener inotify

  killSession (INotifyListener {listenerINotify}) = INo.killINotify listenerINotify

  listen _conf (INotifyListener {listenerINotify}) path actPred chan = do
    path' <- canonicalizeDirPath path
    rawPath <- toRawFilePath path'
    watchStillExistsVar <- newMVar True
    wd <- INo.addWatch listenerINotify varieties rawPath (handleInoEvent actPred chan path' watchStillExistsVar)
    return $ do
      watchStillExists <- readMVar watchStillExistsVar
      when watchStillExists $ INo.removeWatch wd

  listenRecursive conf (INotifyListener {listenerINotify}) initialPath actPred chan = do
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
          maybe (return ()) (mapM_ (\x -> catch (INo.removeWatch x)
                                                (\(e :: SomeException) -> putStrLn ("Error removing watch: " <> show x <> " (" <> show e <> ")")))) mbWds
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
        rawFilePath <- toRawFilePath filePath
        modifyMVar_ wdVar $ \mbWds ->
          -- Atomically add a watch and record its descriptor. Also, check
          -- if the listening task is cancelled, in which case do nothing.
          case mbWds of
            Nothing -> return mbWds
            Just wds -> do
              watchStillExistsVar <- newMVar True
              wd <- INo.addWatch listenerINotify varieties rawFilePath (handler filePath watchStillExistsVar)
              return $ Just (wd:wds)
        where
          handler :: FilePath -> MVar Bool -> INo.Event -> IO ()
          handler baseDir watchStillExistsVar event = do
            -- When a new directory is created, add recursive inotify watches to it
            -- TODO: there's a race condition here; if there are files present in the directory before
            -- we add the watches, we'll miss them. The right thing to do would be to ls the directory
            -- and trigger Added events for everything we find there
            case event of
              (INo.Created True rawDirPath) -> do
                dirPath <- fromRawFilePath rawDirPath
                let newDir = baseDir </> dirPath
                timestampBeforeAddingWatch <- getPOSIXTime
                listenRec newDir wdVar

                -- Find all files/folders that might have been created *after* the timestamp, and hence might have been
                -- missed by the watch
                -- TODO: there's a chance of this generating double events, fix
                files <- S.shelly $ S.find (fromString newDir)
                forM_ files $ \file -> do
                  let newPath = T.unpack $ S.toTextIgnore file
                  fileStatus <- getFileStatus newPath
                  let modTime = modificationTimeHiRes fileStatus
                  when (modTime > timestampBeforeAddingWatch) $ do
                    let isDir = if isDirectory fileStatus then IsDirectory else IsFile
                    let addedEvent = (Added (newDir </> newPath) (posixSecondsToUTCTime timestampBeforeAddingWatch) isDir)
                    when (actPred addedEvent) $ writeChan chan addedEvent

              _ -> return ()

            -- Remove watch when this directory is removed
            case event of
              INo.DeletedSelf -> modifyMVar_ watchStillExistsVar $ const $ return False
              _ -> return ()

            -- Forward all events, including directory create
            handleInoEvent actPred chan baseDir watchStillExistsVar event

  usesPolling = const False
