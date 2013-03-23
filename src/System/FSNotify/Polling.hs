--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

{-# LANGUAGE CPP #-}

module System.FSNotify.Polling
  ( newPollingSession
#ifdef USE_POLLING
  , newSession
#endif
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent
import Control.Exception (mask_)
import Control.Monad (join)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Debug.Trace (trace)
import Filesystem hiding (canonicalizePath)
import Filesystem.Path
import System.FSNotify.Listener
import System.FSNotify.Path (fileDirContents)
import System.FSNotify.Types


data Manager = Manager (MVar WatchMap)
type WatchMap = Map FilePath ThreadId
type FileRec = (FilePath,IsDir)
type PathModMap = Map FileRec UTCTime

#ifdef USE_POLLING
newSession :: IO Session
newSession = newPollingSession
#endif

newPollingSession :: IO Session
newPollingSession = fmap pollSession newPollManager

pollSession :: Manager -> Session
pollSession m = Session (kill m) (clear m) (start m)

newPollManager :: IO Manager
newPollManager = fmap Manager $ newMVar Map.empty

-- stop all watch threads.
kill :: Manager -> IO ()
kill (Manager wm) = modifyMVar wm killMap >>= mapM_ killThread where
    killMap m0 = return (Map.empty,Map.elems m0)

-- stop a particular watch thread.
clear :: Manager -> FilePath -> IO ()
clear (Manager wm) dir = join $ modifyMVar wm remDir where
    remDir m0 = return $ remDir' m0 (Map.lookup dir m0)
    remDir' m0 Nothing = (m0,return())
    remDir' m0 (Just tid) = (Map.delete dir m0, killThread tid)

-- replace existing watch. Will clear the existing one first for
-- reasoning about concurrency, and will only add a new one if 
-- still cleared by the time we try.
start :: Manager -> FilePath -> Action -> IO ()
start m@(Manager wm) dir action = clear m dir >> modifyMVar wm addDir where
    addDir m0 = addDir' m0 (Map.lookup dir m0)
    addDir' m0 Nothing = do
        tid <- forkIO (pollPathInit dir action)
        return (Map.insert dir tid m0, ())
    addDir' m0 _ = return (m0,()) -- concurrent startWatch; the other thread won
   
pollPathInit :: FilePath -> Action -> IO ()
pollPathInit dir action = pollPath dir action =<< pathModMap dir

pathModMap :: FilePath -> IO PathModMap
pathModMap path = do 
    (files,dirs) <- fileDirContents path
    let fs = zip files $ repeat False -- isDir = False
    let ds = zip dirs $ repeat True -- isDir = True
    lst <- mapM withTime (fs ++ ds)
    return (Map.fromList lst) 

withTime :: FileRec -> IO (FileRec,UTCTime)
withTime fr = (,) fr `fmap` getModified (fst fr)

pollPath :: FilePath -> Action -> PathModMap -> IO ()
pollPath dir action oldPathMap = do
    threadDelay 1000000
    newPathMap  <- pathModMap dir
    tNow <- getCurrentTime 
    let lOld = Map.toAscList oldPathMap
        lNew = Map.toAscList newPathMap
        lEvMerge = pollMerge lOld lNew
        lEvents = map (setRemT tNow) lEvMerge
    mapM_ (mask_ . action) lEvents
    pollPath dir action newPathMap

-- Compute events based on differences between two sorted lists.
pollMerge :: [(FileRec,UTCTime)] -> [(FileRec,UTCTime)] -> [Event]
pollMerge lOld        []          = map (event Rem) lOld
pollMerge []          lNew        = map (event Add) lNew
pollMerge lOld@(o:os) lNew@(n:ns) =
    case compare (fst o) (fst n) of
        LT -> event Rem o : pollMerge os lNew
        GT -> event Add n : pollMerge lOld ns
        EQ -> let rest = pollMerge os ns in
              if (snd o == snd n) 
                then rest 
                else event Mod n : rest

event :: AddModRem -> (FileRec,UTCTime) -> Event
event evType ((path,bDir),timestamp) = Event evType path timestamp bDir

-- report removal events at the time of poll rather than their last
-- observed modified time (which might be days ago). Other events 
-- can use their getModified time as it should be much more precise.
setRemT :: UTCTime -> Event -> Event
setRemT tNow (Event Rem path _ bDir) = (Event Rem path tNow bDir)
setRemT _ ev = ev




