--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}

module System.IO.FSNotify
       ( startManager
       , stopManager
       , watch
       , rwatch
       , WatchManager
       ) where

import Prelude hiding (catch)

import Control.Exception
import System.IO.FSNotify.Polling
import System.IO.FSNotify.Types

#ifdef OS_Linux
import System.IO.FSNotify.Linux
#else
#  ifdef OS_Win32
import System.IO.FSNotify.Win32
#  else
#    ifdef OS_Mac
import System.IO.FSNotify.OSX
#    endif
#  endif
#endif

data WatchManager = WatchManager (Either PollManager ListenManager)

startManager :: IO WatchManager
startManager =
  (initSession  >>= return . WatchManager . Right) `catch` (\(_::ListenUnsupportedException) ->
    initSession >>= return . WatchManager . Left)

stopManager :: WatchManager -> IO ()
stopManager (WatchManager wm) =
  case wm of
    Right native -> killSession native
    Left poll    -> killSession poll

watch (WatchManager wm) =
  case wm of
    Right native -> listen native
    Left poll    -> listen poll

rwatch (WatchManager wm) =
  case wm of
    Right native -> rlisten native
    Left poll    -> rlisten poll
