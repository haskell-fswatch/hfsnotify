--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.IO.FSNotify.Linux
       ( FileListener(..)
       , NativeManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, IORef, newIORef, readIORef)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path (findDirs, fp, canonicalizeDirPath)
import System.IO.FSNotify.Types
import qualified System.INotify as INo

type NativeManager = INo.INotify

type IOEvent = IORef Event

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

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> IOEvent -> INo.Event -> IO ()
-- handleInoEvent _       _    basePath _   inoEvent | trace ("Linux: handleInoEvent " ++ show basePath ++ " " ++ show inoEvent) False = undefined
handleInoEvent actPred chan basePath ior inoEvent = do
  currentTime <- getCurrentTime
  let maybeFsnEvent = fsnEvent basePath currentTime inoEvent
  handleEvent actPred chan ior maybeFsnEvent

handleEvent :: ActionPredicate -> EventChannel -> IOEvent -> Maybe Event -> IO ()
-- handleEvent actPred _    _   (Just event) | trace ("Linux: handleEvent " ++ show (actPred event) ++ " " ++ show event) False = undefined
handleEvent actPred chan ior (Just event) =
  when (actPred event) $ do
    lastEvent <- readIORef ior
    when (not $ debounce lastEvent event) (writeChan chan event)
    atomicModifyIORef ior (\_ -> (event, ()))
-- handleEvent _ _ _ Nothing | trace ("Linux handleEvent Nothing") False = undefined
handleEvent _ _ _ Nothing = void

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

instance FileListener INo.INotify where
  initSession = fmap Just INo.initINotify

  killSession = INo.killINotify

  listen iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    ior <- newIORef (Added (fp "") (posixSecondsToUTCTime 0))
    _ <- INo.addWatch iNotify varieties (encodeString path') (handler path' ior)
    void
    where
      handler :: FilePath -> IOEvent -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  -- rlisten iNotify path actPred chan | trace ("Linux: rlisten " ++ fp path) False = undefined
  rlisten iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    paths <- findDirs True path'
    mapM_ pathHandler (path':paths)
    where
      pathHandler :: FilePath -> IO ()
      -- pathHandler filePath _   | trace ("Linux: rlisten pathHandler " ++ show filePath) False = undefined
      pathHandler filePath = do
        ior <- newIORef (Added (fp "") (posixSecondsToUTCTime 0))
        _ <- INo.addWatch iNotify varieties (fp filePath) (handler filePath ior)
        void
        where
          handler :: FilePath -> IOEvent -> INo.Event -> IO ()
          -- handler _ _ event | trace ("Linux: rlisten handler " ++ show event) False = undefined
          handler baseDir _   (INo.Created True dirPath) =
            rlisten iNotify (baseDir </> (fp dirPath)) actPred chan
          handler baseDir ior event                      =
            handleInoEvent actPred chan baseDir ior event
