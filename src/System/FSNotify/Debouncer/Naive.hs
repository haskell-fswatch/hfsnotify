-- |
-- The Naive Debouncer aims to provide a minimalist debouncer logic:
--
-- - it returns at most one event of type @Added@, @Modified@ or @Removed only
-- - it does not try to do any kind of I/O
-- - it does not check that the events are in a coherent order (e.g. it will
-- accept @Added@ followed by @Added@)
-- - it does not throw
--
-- It is based on the following rules:
--
-- - a file was first added, then in the end deleted -> @[]@
-- - a file was first added, then in the end added again -> @[Added]@
-- - a file was first added, then in the end neither added nor deleted ->
-- @[Modified]@
-- - a file was first not added, then in the end not deleted -> @[Modified]@
-- - a file was first not added, then in the end deleted -> @[Removed]@
--
-- By design, the @WatchedDirectoryRemoved@ is folded into @Removed@, while all
-- other events, e.g. `CloseWrite`, are folded into @Modified@.
--
-- The graph of the automaton looks like this (with some artistic license
-- granted to workaround the difficulties of representing graphs in Haddock):
--
-- > --                          "Added"                "Modified"
-- > --                             ^                       ^
-- > --             Added           |           *           |
-- > -- Unknown --------------> AddedFirst ----------> ModifiedThen
-- > --   | |                      ^ |                      |
-- > --   | |                Added | | Removed              | Removed
-- > --   | |                      | v                      ,
-- > --   | |                  RemovedThen <---------------'
-- > --   | |                       |
-- > --   | |                       v
-- > --   | |                   "Nothing"
-- > --   | |
-- > --   | |   Removed
-- > --   | '----------------> RemovedFirst ---> "Removed"
-- > --   \                       ^ |
-- > --    \              Removed | | Added
-- > --     \      *              | v
-- > --      '--------------> ModifiedFirst ---> "Modified"
--
-- - we start from the Unknown state
-- - a @*@ represents all events on the exiting edges of a specific node that
-- aren't explicitly labeled
-- - a node without a @*@-labeled exiting edge has an implicit cycle for @*@
-- - the @Removed@ label includes both @Removed@ and @WatchedDirectoryRemoved@
-- events
-- - the data constructor that a node resolves into once the events have been
-- folded is shown as a node with its name between double quotes

{-# LANGUAGE EmptyDataDecls #-}

module System.FSNotify.Debouncer.Naive (NaiveDebouncer) where

import Data.Foldable
import System.FSNotify.Debouncer.Helper (getPathTimeDir)
import System.FSNotify.Types (Debouncer (..), Event (..))

data InternalState
  = StUnknown
  | StAddedFirst
  | StModifiedFirst
  | StModifiedThen
  | StRemovedFirst
  | StRemovedThen

combine :: InternalState -> Event -> InternalState

combine StUnknown       (Added{})                   = StAddedFirst
combine StUnknown       (Removed{})                 = StRemovedFirst
combine StUnknown       (WatchedDirectoryRemoved{}) = StRemovedFirst
combine StUnknown       (_)                         = StModifiedFirst

combine StAddedFirst    (Removed{})                 = StRemovedThen
combine StAddedFirst    (WatchedDirectoryRemoved{}) = StRemovedThen
combine StAddedFirst    (_)                         = StModifiedThen

combine StModifiedThen  (Removed{})                 = StRemovedThen
combine StModifiedThen  (WatchedDirectoryRemoved{}) = StRemovedThen
combine StModifiedThen  (_)                         = StModifiedThen

combine StRemovedThen   (Added{})                   = StAddedFirst
combine StRemovedThen   (_)                         = StRemovedThen

combine StModifiedFirst (Removed{})                 = StRemovedFirst
combine StModifiedFirst (WatchedDirectoryRemoved{}) = StRemovedFirst
combine StModifiedFirst (_)                         = StModifiedFirst

combine StRemovedFirst  (Added{})                   = StModifiedFirst
combine StRemovedFirst  (_)                         = StRemovedFirst

data NaiveDebouncer

instance Debouncer NaiveDebouncer where
  combineEvents _ _ events =
    let endState = foldl' combine StUnknown events
        (path, time, isDir) = getPathTimeDir $ last events
     in case endState of
          StAddedFirst    -> [Added    path time isDir]
          StModifiedFirst -> [Modified path time isDir]
          StModifiedThen  -> [Modified path time isDir]
          StRemovedFirst  -> [Removed  path time isDir]
          StRemovedThen   -> []
          StUnknown       -> []
