--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}

module System.FSNotify.Linux
       ( newSession
       ) where

-- KNOWN ISSUES:
--
--  Some files are reporting 'Add' events without intermediate removal.
--  (e.g. some of the config state from Chrome browser). Might be missing
--  some essential varieties of inotify events?
--  

import Prelude hiding (FilePath)

import Control.Monad (join)
import Control.Concurrent.MVar
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Path (fp)
import System.FSNotify.Types
import qualified System.INotify as INo

type WatchMap  = Map FilePath INo.WatchDescriptor
data Manager   = Manager INo.INotify (MVar WatchMap)

newSession :: IO Session
newSession = fmap inoSession newManager

inoSession :: Manager -> Session
inoSession m = Session (kill m) (clear m) (start m)

newManager :: IO Manager
newManager = do
    ino <- INo.initINotify
    wm <- newMVar Map.empty
    return (Manager ino wm)

kill :: Manager -> IO ()
kill (Manager ino _) = INo.killINotify ino

clear :: Manager -> FilePath -> IO ()
clear (Manager _ wm) dir = join $ modifyMVar wm clearDir where
    clearDir m0 = return $ clearDir' m0 (Map.lookup dir m0)
    clearDir' m0 Nothing = (m0,return())
    clearDir' m0 (Just wd) = (Map.delete dir m0, INo.removeWatch wd)

start :: Manager -> FilePath -> Action -> IO ()
start (Manager ino wm) dir action = body where
    inoHandler = handleInoEvent dir action
    inoPath = encodeString dir
    body = do 
        wd <- INo.addWatch ino varieties inoPath inoHandler
        modifyMVar_ wm (return . Map.insert dir wd)

varieties :: [INo.EventVariety]
varieties = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]

handleInoEvent :: FilePath -> Action -> INo.Event -> IO ()
handleInoEvent dir action inoEvent = do
    currentTime <- getCurrentTime
    mapM_ action (fsnEvents dir currentTime inoEvent)

-- Note that INo.Closed in this context is "modified" because we listen to
-- CloseWrite events.
fsnEvents :: FilePath -> UTCTime -> INo.Event -> [Event]
fsnEvents basePath timestamp (INo.Created  bDir       name   ) = [Event Add (basePath </> (fp name)) timestamp bDir]
fsnEvents basePath timestamp (INo.Closed   bDir (Just name) _) = [Event Mod (basePath </> (fp name)) timestamp bDir]
fsnEvents basePath timestamp (INo.MovedOut bDir       name  _) = [Event Rem (basePath </> (fp name)) timestamp bDir]
fsnEvents basePath timestamp (INo.MovedIn  bDir       name  _) = [Event Add (basePath </> (fp name)) timestamp bDir]
fsnEvents basePath timestamp (INo.Deleted  bDir       name   ) = [Event Rem (basePath </> (fp name)) timestamp bDir]
fsnEvents _        _         _                                 = []


