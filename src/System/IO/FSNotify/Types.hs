--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses, CPP #-}

module System.IO.FSNotify.Types
       ( PollManager
       , initSession
       , killSession
       , listen
       , rlisten
       ) where

#ifdef OS_Linux
import System.IO.FSNotify.Linux
#else
#ifdef OS_Win32
import System.IO.FSNotify.Win32
#else
#endif
#endif
import System.IO.FSNotify.Polling
