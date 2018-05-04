--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
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
import Data.IORef (atomicModifyIORef, readIORef)
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

type NativeManager = INo.INotify

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

#if __GLASGOW_HASKELL__ >= 804
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
fsnEvents basePath timestamp (INo.Attributes isDir (Just raw)) = fromRawFilePath raw >>= \name -> return [Modified (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Modified isDir (Just raw)) = fromRawFilePath raw >>= \name -> return [Modified (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Created isDir raw) = fromRawFilePath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.MovedOut isDir raw _cookie) = fromRawFilePath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.MovedIn isDir raw _cookie) = fromRawFilePath raw >>= \name -> return [Added (basePath </> name) timestamp isDir]
fsnEvents basePath timestamp (INo.Deleted isDir raw) = fromRawFilePath raw >>= \name -> return [Removed (basePath </> name) timestamp isDir]
fsnEvents _ _ (INo.Ignored) = return []
fsnEvents basePath timestamp inoEvent = return [Unknown basePath timestamp (show inoEvent)]

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> INo.Event -> IO ()
handleInoEvent actPred chan basePath dbp inoEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents basePath currentTime inoEvent
  mapM_ (handleEvent actPred chan dbp) events

handleEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> Event -> IO ()
handleEvent actPred chan dbp event =
  when (actPred event) $ case dbp of
    (Just (DebounceData epsilon ior)) -> do
      lastEvent <- readIORef ior
      unless (debounce epsilon lastEvent event) writeToChan
      atomicModifyIORef ior (const (event, ()))
    Nothing -> writeToChan
  where
    writeToChan = writeChan chan event

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.Attrib, INo.Modify]

instance FileListener INo.INotify where
  initSession = E.catch (fmap Just INo.initINotify) (\(_ :: IOException) -> return Nothing)

  killSession = INo.killINotify

  listen conf iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    dbp <- newDebouncePayload $ confDebounce conf
    rawPath <- toRawFilePath path'
    wd <- INo.addWatch iNotify varieties rawPath (handler path' dbp)
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
          maybe (return ()) (mapM_ (\x -> catch (INo.removeWatch x) (\(_ :: SomeException) -> putStrLn ("Error removing watch: " `mappend` show x)))) mbWds
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
        rawFilePath <- toRawFilePath filePath
        modifyMVar_ wdVar $ \mbWds ->
          -- Atomically add a watch and record its descriptor. Also, check
          -- if the listening task is cancelled, in which case do nothing.
          case mbWds of
            Nothing -> return mbWds
            Just wds -> do
              wd <- INo.addWatch iNotify varieties rawFilePath (handler filePath dbp)
              return $ Just (wd:wds)
        where
          handler :: FilePath -> DebouncePayload -> INo.Event -> IO ()
          handler baseDir dbp event = do
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
                    handleEvent actPred chan dbp (Added (newDir </> newPath) (posixSecondsToUTCTime timestampBeforeAddingWatch) (isDirectory fileStatus))

              _ -> return ()

            -- Remove watch when this directory is removed
            case event of
              (INo.DeletedSelf) -> do
                -- putStrLn "Watched file/folder was deleted! TODO: remove watch."
                return ()
              (INo.Ignored) -> do
                -- putStrLn "Watched file/folder was ignored, which possibly means it was deleted. TODO: remove watch."
                return ()
              _ -> return ()

            -- Forward all events, including directory create
            handleInoEvent actPred chan baseDir dbp event

  usesPolling = const False
