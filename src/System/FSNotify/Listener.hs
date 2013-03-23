--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
module System.FSNotify.Listener
       ( Session(..)
       ) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import System.FSNotify.Types

-- | A Session corresponds to a collection of watch behaviors, up to
-- one per directory. The given FilePaths must be directories. For
-- compatibility across filesystems, an 'initWatch' must replace or
-- stop any existing watch on same directory.
--
--    killSession: halt all watches and relase OS resources.
--    clearWatch: stop watching events on a directory      
--    startWatch: specify watch action for directory events
--
-- All watch actions should be non-blocking and thread-safe. Use of 
-- channels is very appropriate, but not required.
--
-- The possibility of optimizing for recursive listeners exists, but
-- only seems to help for Win32 and seems to hinder compatibility of
-- behavior between systems for removeWatch or overlapping watches.
--
-- NOTE: The Session may assume that all FilePath arguments have
-- been processed by canonicalizeDirPath.
data Session = Session
    { killSession :: IO ()
    , clearWatch :: FilePath -> IO ()
    , startWatch :: FilePath -> Action -> IO ()
    }


