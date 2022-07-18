-- |
-- The Reloader Debouncer has for main objective to signal when a write handle
-- on a file has been closed and it is time to reload it.
--
-- This debouncer is based on the following rules:
--
-- - the @CloseWrite@ event shortcuts and is always returned as final event
-- - events are loosely checked, if two consecutive events do not make sense,
-- it is assumed that some intermediary events have been missed, and the first
-- event is then considered as @StUnknown@
-- - the @ModifyAttributes@ event does not modify the current state
-- - @Unknown@ and unknown (at the time of writing of this code) events always
-- translate into @StUnknown@
--
-- CAVEAT: The CloseWrite event being available only on Linux, other OSes will
-- only signal that the file has been modified. 

{-# LANGUAGE EmptyDataDecls #-}

module System.FSNotify.Debouncer.Reloader (ReloaderDebouncer) where

import Data.Foldable
import System.FSNotify.Debouncer.Helper (getPathTimeDir)
import System.FSNotify.Types (Debouncer (..), Event (..))

data InternalState
  = StUnknown
  | StAdded
  | StBeingModified
  | StModified
  | StRemoved

combine :: InternalState -> Event -> InternalState

combine StUnknown       (Added{})                   = StAdded
combine StUnknown       (Modified{})                = StBeingModified
combine StUnknown       (ModifiedAttributes{})      = StUnknown
combine StUnknown       (Removed{})                 = StRemoved
combine StUnknown       (WatchedDirectoryRemoved{}) = StRemoved
combine StUnknown       (CloseWrite{})              = StModified
combine StUnknown       (_)                         = StUnknown

combine StAdded         (Added{})                   = StAdded
combine StAdded         (Modified{})                = StBeingModified
combine StAdded         (ModifiedAttributes{})      = StAdded
combine StAdded         (Removed{})                 = StRemoved
combine StAdded         (WatchedDirectoryRemoved{}) = StRemoved
combine StAdded         (CloseWrite{})              = StModified
combine StAdded         (_)                         = StUnknown

combine StBeingModified (Added{})                   = StBeingModified
combine StBeingModified (Modified{})                = StBeingModified
combine StBeingModified (ModifiedAttributes{})      = StBeingModified
combine StBeingModified (Removed{})                 = StRemoved
combine StBeingModified (WatchedDirectoryRemoved{}) = StRemoved
combine StBeingModified (CloseWrite{})              = StModified
combine StBeingModified (_)                         = StUnknown

combine StModified      (_)                         = StModified

combine StRemoved       (Added{})                   = StAdded
combine StRemoved       (Modified{})                = StBeingModified
combine StRemoved       (ModifiedAttributes{})      = StRemoved
combine StRemoved       (Removed{})                 = StRemoved
combine StRemoved       (WatchedDirectoryRemoved{}) = StRemoved
combine StRemoved       (CloseWrite{})              = StModified
combine StRemoved       (_)                         = StUnknown

data ReloaderDebouncer

instance Debouncer ReloaderDebouncer where
  combineEvents _ _ events =
    let endState = foldl' combine StUnknown events
        (path, time, isDir) = getPathTimeDir $ last events
     in case endState of
          StAdded         -> [Added      path time isDir]
          StBeingModified -> [Modified   path time isDir]
          StModified      -> [CloseWrite path time isDir]
          StRemoved       -> [Removed    path time isDir]
          StUnknown       -> []
