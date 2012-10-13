module System.FSNotify.React 
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
doAllEvents action event =
  case event of
    Added    f _ -> action f
    Modified f _ -> action f
    Removed  f _ -> action f

existsEvents :: (FilePath -> Bool) -> (Event -> Bool)
existsEvents filt event =
  case event of
    Added    f _ -> filt f
    Modified f _ -> filt f
    Removed  _ _ -> False

allEvents :: (FilePath -> Bool) -> (Event -> Bool)
allEvents filt event =
  case event of
    Added    f _ -> filt f
    Modified f _ -> filt f
    Removed  f _ -> filt f
