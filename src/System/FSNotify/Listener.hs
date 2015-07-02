--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Listener
       ( debounce
       , epsilonDefault
       , FileListener(..)
       , StopListening
       , newDebouncePayload
       ) where

import Prelude hiding (FilePath)

import Data.IORef (newIORef)
import Data.Time (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.FilePath
import System.FSNotify.Types

-- | An action that cancels a watching/listening job
type StopListening = IO ()

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
  listen :: WatchConfig -> sessionType -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening

  -- | Listen for file events associated with all the contents of a directory.
  -- Listening for events associated with all the contents of a directory will
  -- report events associated with files within the specified directory and its
  -- subdirectories.
  listenRecursive :: WatchConfig -> sessionType -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening

  -- | Does this manager use polling?
  usesPolling :: sessionType -> Bool

-- | The default maximum difference (exclusive, in seconds) for two
-- events to be considered as occuring "at the same time".
epsilonDefault :: NominalDiffTime
epsilonDefault = 0.001

-- | The default event that provides a basis for comparison.
eventDefault :: Event
eventDefault = Added "" (posixSecondsToUTCTime 0)

-- | A predicate indicating whether two events may be considered "the same
-- event". This predicate is applied to the most recent dispatched event and
-- the current event after the client-specified ActionPredicate is applied,
-- before the event is dispatched.
debounce :: NominalDiffTime -> Event -> Event -> Bool
debounce epsilon e1 e2 =
  eventPath e1 == eventPath e2 && timeDiff > -epsilon && timeDiff < epsilon
  where
    timeDiff = diffUTCTime (eventTime e2) (eventTime e1)

-- | Produces a fresh data payload used for debouncing events in a
-- handler.
newDebouncePayload :: Debounce -> IO DebouncePayload
newDebouncePayload DebounceDefault    = newIORef eventDefault >>= return . Just . DebounceData epsilonDefault
newDebouncePayload (Debounce epsilon) = newIORef eventDefault >>= return . Just . DebounceData epsilon
newDebouncePayload NoDebounce         = return Nothing
