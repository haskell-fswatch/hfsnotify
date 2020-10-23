{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.FSNotify.Listener (
  FileListener(..)
  , StopListening
  , ListenFn
  ) where

import Data.Text
import Prelude hiding (FilePath)
import System.FSNotify.Types
import System.FilePath

-- | An action that cancels a watching/listening job
type StopListening = IO ()

type ListenFn sessionType argType = FileListener sessionType argType => WatchConfig -> sessionType -> FilePath -> ActionPredicate -> EventCallback -> IO StopListening

-- | A typeclass that imposes structure on watch managers capable of listening
-- for events, or simulated listening for events.
class FileListener sessionType argType | sessionType -> argType where
  -- | Initialize a file listener instance.
  initSession :: argType -> IO (Either Text sessionType)
  -- ^ An initialized file listener, or a reason why one wasn't able to start.

  -- | Kill a file listener instance.
  -- This will immediately stop acting on events for all directories being
  -- watched.
  killSession :: sessionType -> IO ()

  -- | Listen for file events associated with the immediate contents of a directory.
  -- Listening for events associated with immediate contents of a directory will
  -- only report events associated with files within the specified directory, and
  -- not files within its subdirectories.
  listen :: ListenFn sessionType argType

  -- | Listen for file events associated with all the contents of a directory.
  -- Listening for events associated with all the contents of a directory will
  -- report events associated with files within the specified directory and its
  -- subdirectories.
  listenRecursive :: ListenFn sessionType argType

  -- | Does this manager use polling?
  usesPolling :: sessionType -> Bool
