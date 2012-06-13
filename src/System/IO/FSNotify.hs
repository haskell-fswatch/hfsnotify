--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE CPP, ScopedTypeVariables #-}
module System.IO.FSNotify
( startManager
, stopManager
, watch
, WatchManager
) where

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

import System.IO.FSNotify.Polling

import Prelude hiding (catch)
import Control.Exception

data WatchManager = WatchManager (Either PollManager ListenManager)

startManager :: IO WatchManager
startManager =
  (initSession >>= return . WatchManager . Right) `catch` (\(_::SomeException) ->
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
