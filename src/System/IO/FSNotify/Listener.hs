--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Listener
       ( FileListener(..)
       ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)
import System.IO.FSNotify.Types

-- | A typeclass that imposes structure on watch managers capable of listening
-- for events, or simulated listening for events.
class FileListener sessionType where
  -- | Initialize a file listener instance.
  initSession :: IO (Maybe sessionType) -- ^ Just an initialized file listener,
                                        --   or Nothing if this file listener
                                        --   cannot be supported.

  -- | Kill a file listener instance.
  -- This will immediately stop acting on events for all directories being
  -- watched.
  killSession :: sessionType -> IO ()

  -- | Listen for file events associated with the immediate contents of a directory.
  -- Listening for events associated with immediate contents of a directory will
  -- only report events associated with files within the specified directory, and
  -- not files within its subdirectories.
  listen :: sessionType -> FilePath -> ActionPredicate -> EventChannel -> IO ()

  -- | Listen for file events associated with all the contents of a directory.
  -- Listening for events associated with all the contents of a directory will
  -- report events associated with files within the specified directory and its
  -- subdirectories.
  rlisten :: sessionType -> FilePath -> ActionPredicate -> EventChannel -> IO ()
