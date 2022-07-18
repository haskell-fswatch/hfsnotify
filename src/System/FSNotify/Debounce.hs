{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module System.FSNotify.Debounce (
  makeDebouncedAction,
  Debouncer(..),
  ) where

import Control.Debounce
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import System.FSNotify.Types


-- | This function will take an 'Action' and return a debounced version of the 'Action'.
-- The debouncing uses a simple state machine to combine events.
-- For example, if a file is modified and then removed during the debouncing period, you'll get a single 'Removed' event.
-- If a file is added and then removed, you'll get no event. And so on.
-- Warning: if you use an 'ActionPredicate' in conjunction with this, be careful that it
-- doesn't filter out some event types for a given filepath and not others, or else the state
-- machine could become confused.
makeDebouncedAction :: Debouncer a => Int -> Action -> a -> IO Action
makeDebouncedAction freq baseAction debouncer = do
  eventMap :: IORef (M.Map FilePath [Event]) <- newIORef mempty

  let flushEvents = do
        eventsToFlush <- atomicModifyIORef eventMap $ \m -> (mempty, m)
        forM_ (M.toList eventsToFlush) $ \(path, events) -> do
          let combinedEvents = combineEvents debouncer path events
          mapM_ baseAction combinedEvents

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
