--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Types
       ( Event(..)
       , ActionPredicate
       , Action
       , act
       ) where

import Prelude hiding (FilePath)

import Filesystem.Path.CurrentOS
import System.IO hiding (FilePath)

-- | A file event reported by a file watcher.
data Event =
    Added    FilePath
  | Modified FilePath
  | Removed  FilePath
  deriving (Show)

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | Predicate for "always act".
act :: ActionPredicate
act event = True
