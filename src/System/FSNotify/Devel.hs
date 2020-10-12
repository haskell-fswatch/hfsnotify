{-# LANGUAGE FlexibleContexts #-}

-- | Some additional functions on top of "System.FSNotify".
--
-- Example of compiling scss files with compass
--
-- @
-- compass :: WatchManager -> FilePath -> m ()
-- compass man dir = do
--  putStrLn $ "compass " ++ encodeString dir
--  treeExtExists man dir "scss" $ \fp ->
--    when ("deploy" `notElem` splitDirectories fp) $ do
--     let d = encodeString $ head (splitDirectories rel)
--     system "cd " ++ d ++ "&& bundle exec compass compile"
--  return ()
-- @

{-# LANGUAGE NamedFieldPuns #-}

module System.FSNotify.Devel
  ( treeExtAny, treeExtExists,
    doAllEvents,
    allEvents, existsEvents
  ) where

import Data.Text
import Prelude hiding (FilePath)
import System.FSNotify
import System.FSNotify.Path (hasThisExtension)
import System.FilePath

-- | In the given directory tree, watch for any 'Added' and 'Modified'
-- events (but ignore 'Removed' events) for files with the given file
-- extension
treeExtExists :: WatchManager
              -> FilePath -- ^ Directory to watch
              -> Text -- ^ extension
              -> (FilePath -> IO ()) -- ^ action to run on file
              -> IO StopListening
treeExtExists man dir ext action =
  watchTree man dir (existsEvents $ flip hasThisExtension ext) (doAllEvents action)

-- | In the given directory tree, watch for any events for files with the
-- given file extension
treeExtAny :: WatchManager
           -> FilePath -- ^ Directory to watch
           -> Text -- ^ extension
           -> (FilePath -> IO ()) -- ^ action to run on file
           -> IO StopListening
treeExtAny man dir ext action =
  watchTree man dir (allEvents $ flip hasThisExtension ext) (doAllEvents action)

-- | Turn a 'FilePath' callback into an 'Event' callback that ignores the
-- 'Event' type and timestamp
doAllEvents :: Monad m => (FilePath -> m ()) -> Event -> m ()
doAllEvents action = action . eventPath

-- | Turn a 'FilePath' predicate into an 'Event' predicate that accepts
-- only 'Added' and 'Modified' event types
existsEvents :: (FilePath -> Bool) -> (Event -> Bool)
existsEvents filt event =
  case event of
    Added {eventPath} -> filt eventPath
    Modified {eventPath} -> filt eventPath
    WatchedDirectoryRemoved {} -> False
    Removed {} -> False
    Unknown {} -> False

-- | Turn a 'FilePath' predicate into an 'Event' predicate that accepts
-- any event types
allEvents :: (FilePath -> Bool) -> (Event -> Bool)
allEvents filt = filt . eventPath
