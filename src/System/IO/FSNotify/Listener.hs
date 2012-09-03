--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Listener
       ( debounce
       , epsilon
       , FileListener(..)
       ) where

import Prelude hiding (FilePath)

import Data.Time (diffUTCTime, NominalDiffTime)
import Filesystem.Path.CurrentOS
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

-- | The maximum difference (exclusive, in seconds) for two events to be
-- considered as occuring "at the same time".
epsilon :: NominalDiffTime
epsilon = 0.001

-- | A predicate indicating whether two events may be considered "the same
-- event". This predicate is applied to the most recent dispatched event and
-- the current event after the client-specified ActionPredicate is applied,
-- before the event is dispatched.
debounce :: Event -> Event -> Bool
debounce e1 e2 =
  eventPath e1 == eventPath e2 && timeDiff > -epsilon && timeDiff < epsilon
  where
    timeDiff = diffUTCTime (eventTime e2) (eventTime e1)
