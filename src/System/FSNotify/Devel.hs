module System.FSNotify.Devel
  ( treeExtAny, treeExtExists,
    doAllEvents,
    allEvents, existsEvents
  ) where

import Prelude hiding (FilePath, catch)

import Data.Text
import Filesystem.Path.CurrentOS
import System.FSNotify
-- import System.FSNotify.Path (fp)

-- | Example of compiling scss files with compass
-- @
-- compass :: WatchManager -> FilePath -> IO ()
-- compass man dir = do
--  putStrLn $ "compass " ++ encodeString dir
--  treeExtExists man dir "scss" $ \fp ->
--    when ("deploy" `notElem` splitDirectories fp) $ do
--     let d = encodeString $ head (splitDirectories rel)
--     system "cd " ++ d ++ "&& bundle exec compass compile"
--  return ()
-- @

-- | In the given directory tree,
-- watch for any Added and Modified events (but ignore Removed events)
-- for files with the given file extension
-- perform the given action
treeExtExists :: WatchManager
         -> FilePath -- ^ Directory to watch
         -> Text -- ^ extension
         -> (FilePath -> IO ()) -- ^ action to run on file
         -> IO ()
treeExtExists man dir ext action =
  watchTree man dir (existsEvents $ flip hasExtension ext) (doAllEvents action)

-- | In the given directory tree,
-- for files with the given file extension
-- perform the given action
treeExtAny :: WatchManager
         -> FilePath -- ^ Directory to watch
         -> Text -- ^ extension
         -> (FilePath -> IO ()) -- ^ action to run on file
         -> IO ()
treeExtAny man dir ext action =
  watchTree man dir (existsEvents $ flip hasExtension ext) (doAllEvents action)

doAllEvents :: Monad m => (FilePath -> m ()) -> Event -> m ()
doAllEvents action = action . eventPath

existsEvents :: (FilePath -> Bool) -> (Event -> Bool)
existsEvents filt event = (isExistsEvent event) && (filt (eventPath event))

allEvents :: (FilePath -> Bool) -> (Event -> Bool)
allEvents filt = filt . eventPath

