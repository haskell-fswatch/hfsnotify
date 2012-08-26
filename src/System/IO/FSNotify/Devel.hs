--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Devel
       ( compileTrees
       , compileTree
       , compilePaths
       , compilePath
       , compilePatternMulti
       , compilePattern
       , compileMulti
       , compileBasic
       ) where

import Prelude hiding (FilePath, catch)

import Data.Text
import Filesystem.Path.CurrentOS
import System.FilePath.Glob (match, Pattern)
import System.IO.FSNotify
import System.IO.FSNotify.Path (fp)
import System.IO.FSNotify.Types

compileTrees :: WatchManager
             -> [FilePath] -- ^ Directories to watch
             -> ActionPredicate -- ^ Predicate for event filtering
             -> (FilePath -> IO ()) -- ^ Compile action
             -> IO ()
compileTrees man dirs actPred action =
  mapM_ createWatch dirs
  where
    createWatch :: FilePath -> IO ()
    createWatch dir = watchTree man dir actPred compile
    compile :: Action
    compile event = action $ eventPath event

compileTree :: WatchManager
            -> FilePath -- ^ Directory to watch
            -> ActionPredicate -- ^ Predicate for event filtering
            -> (FilePath -> IO ()) -- ^ Compile action
            -> IO ()
compileTree man dir = compileTrees man [dir]

compilePaths :: WatchManager
             -> [FilePath] -- ^ Directories to watch
             -> (FilePath -> Bool) -- ^ Predicate for filtering by file name
             -> (FilePath -> IO ()) -- ^ Compile action
             -> IO ()
compilePaths man dirs pathPred action =
  compileTrees man dirs predicate action
  where
    predicate :: ActionPredicate
    predicate = compilePredicate pathPred

compilePath :: WatchManager
             -> FilePath -- ^ Directories to watch
             -> (FilePath -> Bool) -- ^ Predicate for filtering by file name
             -> (FilePath -> IO ()) -- ^ Compile action
             -> IO ()
compilePath man dir = compilePaths man [dir]

compilePatternMulti :: WatchManager
                    -> [FilePath] -- ^ Directories to watch
                    -> Pattern -- ^ Pattern to match against event
                    -> (FilePath -> IO ()) -- ^ Compile action
                    -> IO ()
compilePatternMulti man dirs pat action =
  compilePaths man dirs patFilter action
  where
    patFilter :: FilePath -> Bool
    patFilter = \path -> match pat (fp path)

compilePattern :: WatchManager
                -> FilePath -- ^ Directory to watch
                -> Pattern -- ^ Pattern to match against event
                -> (FilePath -> IO ()) -- ^ Compile action
                -> IO ()
compilePattern man dir =
  compilePatternMulti man [dir]

compileMulti :: WatchManager
              -> [FilePath] -- ^ Directories to watch
              -> Text -- ^ Extension to watch (old extension)
              -> Text -- ^ Output file extension (new extension)
              -> (FilePath -> FilePath -> IO ()) -- ^ Compile action
              -> IO ()
compileMulti man dirs oldExt newExt action =
  compilePaths man dirs extFilter compile
  where
    extFilter :: FilePath -> Bool
    extFilter = flip hasExtension oldExt
    compile :: FilePath -> IO ()
    compile path = action path $ convert path
    convert :: FilePath -> FilePath
    convert = flip replaceExtension newExt

compileBasic :: WatchManager
              -> FilePath -- ^ Directory to watch
              -> Text -- ^ Extension to watch (old extension)
              -> Text -- ^ Output file extension
              -> (FilePath -> FilePath -> IO ()) -- ^ Compile action
              -> IO ()
compileBasic man dir = compileMulti man [dir]

compilePredicate :: (FilePath -> Bool) -> ActionPredicate
compilePredicate pathPred = \event ->
  case event of
    Added    path _ -> pathPred path
    Modified path _ -> pathPred path
    _               -> False
