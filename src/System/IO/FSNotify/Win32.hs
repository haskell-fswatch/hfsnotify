--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Win32
       ( FileListener(..)
       , ListenManager
       ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify.Path
import System.IO.FSNotify.Types
import qualified System.Win32.Notify as WNo

type ListenManager = WNo.WatchManager

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

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly throw
  -- a ListenUnsupportedException if the calls we need are not available.
  -- This will require that API version information be exposed by Win32-notify.
  initSession = WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen watchManager path actPred action = do
    WNo.watchDirectory watchManager (str path) False varieties handler
    return ()
    where
      varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
      handler :: WNo.Event -> IO ()
      handler = handleWNoEvent actPred action

  rlisten watchManager path actPred action = do
    WNo.watchDirectory watchManager (str path) True varieties handler
    return ()
    where
      varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
      handler :: WNo.Event -> IO ()
      handler = handleWNoEvent actPred action
