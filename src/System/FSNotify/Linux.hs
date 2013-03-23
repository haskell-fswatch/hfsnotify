--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE DeriveDataTypeable #-}

module System.FSNotify.Linux
       ( newSession
       ) where

import Prelude hiding (FilePath)

import Control.Exception
import Control.Monad (join)
import Data.IORef 
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import System.FSNotify.Listener
import System.FSNotify.Path (fp)
import System.FSNotify.Types
import qualified System.INotify as INo

type WatchMap  = Map FilePath INo.WatchDescriptor
data Manager   = Manager INo.INotify (IORef WatchMap)

newSession :: IO Session
newSession = fmap inoSession newManager

inoSession :: Manager -> Session
inoSession m = Session (kill m) (clear m) (start m)

newManager :: IO Manager
newManager = do
    ino <- INo.initINotify
    wm <- newIORef Map.empty
    return (Manager ino wm)

kill :: Manager -> IO ()
kill (Manager ino _) = INo.killINotify ino

clear :: Manager -> FilePath -> IO ()
clear (Manager _ wm) dir = join $ atomicModifyIORef wm clearDir where
    clearDir m0 = clearDir' m0 (Map.lookup dir m0)
    clearDir' m0 Nothing = (m0,return())
    clearDir' m0 (Just wd) = (Map.delete dir m0, INo.removeWatch wd)

-- this one's a little tricky. I want to record the INo.WatchDescriptor
-- atomically with generating it. Fortunately, I don't use the wd, except
-- to record it, so I can leverage Control.Monad.Fix.mfix and Haskell's
-- laziness to tie the knot. (I suppose I could have used an MVar to make
-- this easy. But I prefer wait-free solutions where feasible.)
start :: Manager -> FilePath -> Action -> IO ()
start (Manager ino wm) dir action = do
    let inoHandler = handleInoEvent dir action
    let inoPath = encodeString dir
    wd <- INo.addWatch ino varieties inoPath inoHandler
    join (atomicModifyIORef wm (addWatch dir wd))

addWatch :: FilePath -> INo.WatchDescriptor -> WatchMap -> (WatchMap,IO())
addWatch dir wd m0 = addWatch' (Map.lookup dir m0) where
    addWatch' Nothing = (Map.insert dir wd m0, return ())
    addWatch' (Just wd0) | (wd0 == wd) = (m0, return ())
    addWatch' (Just wd0) = assert False $ (m0, INo.removeWatch wd0)
        -- the 'assert False' is because according to the inotify manual,
        -- a watch descriptor is supposed to be unique per pathname.
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


