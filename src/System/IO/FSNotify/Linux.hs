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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.IO.FSNotify.Listener
import System.IO.FSNotify.Path (findDirs, fp, canonicalizeDirPath)
import System.IO.FSNotify.Types
import qualified System.INotify as INo

type NativeManager = INo.INotify

data EventVarietyMismatchException = EventVarietyMismatchException deriving (Show, Typeable)
instance Exception EventVarietyMismatchException

-- Note that INo.Closed in this context is "modified" because we listen to
-- CloseWrite events.
fsnEvent :: FilePath -> UTCTime -> INo.Event -> Maybe Event
fsnEvent basePath timestamp (INo.Created  False       name   ) = Just (Added    (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.Closed   False (Just name) _) = Just (Modified (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.MovedOut False       name  _) = Just (Removed  (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.MovedIn  False       name  _) = Just (Added    (basePath </> (fp name)) timestamp)
fsnEvent basePath timestamp (INo.Deleted  False       name   ) = Just (Removed  (basePath </> (fp name)) timestamp)
fsnEvent _        _         _                                  = Nothing

handleInoEvent :: ActionPredicate -> EventChannel -> FilePath -> INo.Event -> IO ()
-- handleInoEvent _       _    basePath inoEvent | trace ("Linux: handleInoEvent " ++ show basePath ++ " " ++ show inoEvent) False = undefined
handleInoEvent actPred chan basePath inoEvent = do
  currentTime <- getCurrentTime
  let maybeFsnEvent = fsnEvent basePath currentTime inoEvent
  handleEvent actPred chan maybeFsnEvent

handleEvent :: ActionPredicate -> EventChannel -> Maybe Event -> IO ()
-- handleEvent actPred _    (Just event) | trace ("Linux: handleEvent " ++ show (actPred event) ++ " " ++ show event) False = undefined
handleEvent actPred chan (Just event)
  | actPred event       = writeChan chan event
  | otherwise           = return ()
-- handleEvent _ _ Nothing | trace ("Linux handleEvent Nothing") False = undefined
handleEvent _ _ Nothing = return ()

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

instance FileListener INo.INotify where
  initSession = fmap Just INo.initINotify

  killSession = INo.killINotify

  listen iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    INo.addWatch iNotify varieties (encodeString path') (handler path')
    return ()
    where
      handler :: FilePath -> INo.Event -> IO ()
      handler = handleInoEvent actPred chan

  -- rlisten iNotify path actPred chan | trace ("Linux: rlisten " ++ fp path) False = undefined
  rlisten iNotify path actPred chan = do
    path' <- canonicalizeDirPath path
    paths <- findDirs True path'
    mapM_ pathHandler (path':paths)
    where
      pathHandler :: FilePath -> IO ()
      -- pathHandler filePath | trace ("Linux: rlisten pathHandler " ++ show filePath) False = undefined
      pathHandler filePath = do
        INo.addWatch iNotify varieties (fp filePath) (handler filePath)
        return ()
        where
          handler :: FilePath -> INo.Event -> IO ()
          -- handler _ event | trace ("Linux: rlisten handler " ++ show event) False = undefined
          handler baseDir (INo.Created True dirPath) =
            rlisten iNotify (baseDir </> (fp dirPath)) actPred chan
          handler baseDir event                      = handleInoEvent actPred chan baseDir event
