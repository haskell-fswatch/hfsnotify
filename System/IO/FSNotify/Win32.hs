--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}

module System.IO.FSNotify.Win32
  (
  ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify
import qualified System.Win32.Notify as WNo

-- Helper functions for dealing with String vs. FileSystem.Path.CurrentOS.FilePath
fp :: String -> FilePath
fp = decodeString
str :: FilePath -> String
str = encodeString

fsnEvents :: WNo.Event -> [Event]
fsnEvents (WNo.Created  False name)                   = [Added (fp name)]
fsnEvents (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName), Added (fp newName)]
fsnEvents (WNo.Renamed  False Nothing newName)        = [Added (fp newName)]
fsnEvents (WNo.Modified False (Just name))            = [Modified (fp name)]
fsnEvents (WNo.Deleted  False name)                   = [Removed (fp name)]
fsnEvents _                                           = []

handleWNoEvent :: ActionPredicate -> Action -> WNo.Event -> IO ()
handleWNoEvent actPred action inoEvent = do
  mapM_ (handleEvent actPred action) (fsnEvents inoEvent)
  return ()
handleEvent :: ActionPredicate -> Action -> Event -> IO ()
handleEvent actPred action event = if actPred event then action event else return ()

instance ListenerSession WNo.WatchManager where
  initSession = WNo.initWatchManager
  killSession = WNo.killWatchManager

instance FileListener WNo.WatchManager WNo.WatchId where
  listen watchManager path actPred action =
    WNo.watchDirectory watchManager (str path) False varieties handler
    where
      varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
      handler :: WNo.Event -> IO ()
      handler = handleWNoEvent actPred action

  rlisten watchManager path actPred action = do
    watchIdHandle <- WNo.watchDirectory watchManager (str path) True varieties handler
    return [watchIdHandle]
    where
      varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
      handler :: WNo.Event -> IO ()
      handler = handleWNoEvent actPred action
