--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Win32
       ( FileListener(..)
       , ListenManager
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify.Listener
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

handleWNoEvent :: ActionPredicate -> EventChannel -> WNo.Event -> IO ()
handleWNoEvent actPred chan inoEvent = do
  mapM_ (handleEvent actPred chan) (fsnEvents inoEvent)
  return ()
handleEvent :: ActionPredicate -> EventChannel -> Event -> IO ()
handleEvent actPred chan event = when (actPred event) writeChan event chan

instance FileListener WNo.WatchManager where
  -- TODO: This should actually lookup a Windows API version and possibly return
  -- Nothing the calls we need are not available. This will require that API
  -- version information be exposed by Win32-notify.
  initSession = fmap Just WNo.initWatchManager

  killSession = WNo.killWatchManager

  listen watchManager path actPred chan = do
    WNo.watchDirectory watchManager (str path) False varieties handler
    return ()

  rlisten watchManager path actPred chan = do
    WNo.watchDirectory watchManager (str path) True varieties handler
    return ()

handler :: WNo.Event -> IO ()
handler = handleWNoEvent actPred chan

varieties :: [WNo.Event]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]
