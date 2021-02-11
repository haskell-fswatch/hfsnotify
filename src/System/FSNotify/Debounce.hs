{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.FSNotify.Debounce (
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
            Nothing -> return ()
            Just x -> baseAction x

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
combineEvents :: Bool -> [Event] -> Maybe Event
combineEvents fileExists = snd . foldl logic (fileExists, Nothing)
  where
    -- Combining logic
    logic (True, Nothing) x@(Modified {}) = (True, Just x)
    logic (True, Nothing) x@(ModifiedAttributes {}) = (True, Just x)
    logic (True, Nothing) x@(Removed {}) = (False, Just x)
    logic (False, Nothing) x@(Added {}) = (True, Just x)
    logic (True, Just (Added {})) x@(Modified {}) = (True, Just x)
    logic (True, Just (Added {})) x@(ModifiedAttributes {}) = (True, Just x)
    logic (True, Just (Added {})) (Removed {}) = (False, Nothing)

    -- TODO: We might actually need to be able to track "modified" and "modified attributes" simultaneously
    logic (True, Just (Modified {})) x@(Modified {}) = (True, Just x)
    logic (True, Just (Modified {})) x@(ModifiedAttributes {}) = (True, Just x)
    logic (True, Just (ModifiedAttributes {})) x@(Modified {}) = (True, Just x)
    logic (True, Just (ModifiedAttributes {})) x@(ModifiedAttributes {}) = (True, Just x)

    logic (True, Just (Modified {})) x@(Removed {}) = (False, Just x)
    logic (False, Just (Removed {})) x@(Added {}) = (True, Just x)

    -- If you see these, it means your notification manager is broken
    logic (True , _) (Added {}) = error "File already exists"
    logic (False , _) (Modified {}) = error "File does not exist"
    logic (False , _) (ModifiedAttributes {}) = error "File does not exist"
    logic (False , _) (Removed {}) = error "File does not exist"

    -- If you see these, it means this function's logic is broken
    logic (True, Just (Removed {})) _ = error "Impossible"
    logic (False, Just (Added {})) _ = error "Impossible"
    logic (False, Just (Modified {})) _ = error "Impossible"
