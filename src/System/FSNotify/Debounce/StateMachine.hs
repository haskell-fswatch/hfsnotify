{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.FSNotify.Debounce.StateMachine (
  makeDebouncedAction
  ) where

import Control.Debounce
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import System.Directory
import System.FSNotify.Types


-- | This function will take an 'Action' and return a debounced version of the 'Action'.
-- The debouncing uses a simple state machine to combine events.
-- For example, if a file is modified and then removed during the debouncing period, you'll get a single 'Removed' event.
-- If a file is added and then removed, you'll get no event. And so on.
-- Warning: if you use an 'ActionPredicate' in conjunction with this, be careful that it
-- doesn't filter out some event types for a given filepath and not others, or else the state
-- machine could become confused.
makeDebouncedAction :: Int -> Action -> IO Action
makeDebouncedAction freq baseAction = do
  eventMap :: IORef (M.Map FilePath [Event]) <- newIORef mempty

  let flushEvents = do
        eventsToFlush <- atomicModifyIORef eventMap $ \m -> (mempty, m)
        forM_ (M.toList eventsToFlush) $ \(path, events) -> do
          fileExists <- doesFileExist path
          case combineEvents fileExists events of
            -- Debouncing logic failed: fall back to reporting all actions
            Left () -> forM_ events baseAction
            -- No event to fire. For example: [Added, Removed]
            Right Nothing -> return ()
            -- Debounced event to fire
            Right (Just x) -> baseAction x

  flusher <- mkDebounce $ defaultDebounceSettings {
    debounceAction = flushEvents
    , debounceFreq = freq
    , debounceEdge = trailingEdge
    }

  return $ \event -> do
    atomicModifyIORef eventMap $ (, ()) . (
      flip M.alter (eventPath event) $ \case
          Just xs -> Just (event : xs)
          Nothing -> Just [event]
      )

    flusher


-- It is assumed that all events passed into this function have the same eventPath
combineEvents :: Bool -> [Event] -> Either () (Maybe Event)
combineEvents fileExists events = snd <$> foldM logic (fileExists, Nothing) events
  where
    logic :: (Bool, Maybe Event) -> Event -> Either () (Bool, Maybe Event)

    -- * File exists, no previous event

    -- Invalid: Added and WatchedDirectoryRemoved
    logic (True, Nothing) x@(Added {}) = Left ()
    logic (True, Nothing) x@(WatchedDirectoryRemoved {}) = Left ()
    -- Removed makes the file nonexistent
    logic (True, Nothing) x@(Removed {}) = Right (False, Just x)
    -- Everything else just becomes the current event
    logic (True, Nothing) x = Right (True, Just x)

    -- * File doesn't exist, no previous event

    -- Added makes the file exist
    logic (False, Nothing) x@(Added {}) = Right (True, Just x)
    -- Invalid: everything else
    logic (False, Nothing) _ = Left ()

    -- * File exists, with previous event

    -- Added + removed = gone
    logic (True, Just (Added {})) (Removed {}) = Right (False, Nothing)
    -- Added + modified = modified
    logic (True, Just (Added {})) x@(Modified {}) = Right (True, Just x)
    logic (True, Just (Added {})) x@(ModifiedAttributes {}) = Right (True, Just x)

    -- * File doesn't exist, with previous event

    -- Removed + added = modified?
    -- logic (False, Just (Removed {})) x@(Added {}) = (True, Just x)

    -- -- TODO: We might actually need to be able to track "modified" and "modified attributes" simultaneously
    -- logic (True, Just (Modified {})) x@(Modified {}) = (True, Just x)
    -- logic (True, Just (Modified {})) x@(ModifiedAttributes {}) = (True, Just x)
    -- logic (True, Just (ModifiedAttributes {})) x@(Modified {}) = (True, Just x)
    -- logic (True, Just (ModifiedAttributes {})) x@(ModifiedAttributes {}) = (True, Just x)

    -- logic (True, Just (Modified {})) x@(Removed {}) = (False, Just x)
    -- logic (False, Just (Removed {})) x@(Added {}) = (True, Just x)

    -- -- If you see these, it means your notification manager is broken
    -- logic (True , _) (Added {}) = error "File already exists"
    -- logic (False , _) (Modified {}) = error "File does not exist"
    -- logic (False , _) (ModifiedAttributes {}) = error "File does not exist"
    -- logic (False , _) (Removed {}) = error "File does not exist"

    -- -- If you see these, it means this function's logic is broken
    -- logic (True, Just (Removed {})) _ = error "Impossible"
    -- logic (False, Just (Added {})) _ = error "Impossible"
    -- logic (False, Just (Modified {})) _ = error "Impossible"
