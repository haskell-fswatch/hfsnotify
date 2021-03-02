--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module System.FSNotify.Path
       ( findFiles
       , findFilesAndDirs
       , canonicalizeDirPath
       , canonicalizePath
       , hasThisExtension
       ) where

import Control.Monad
import qualified Data.Text as T
import Prelude hiding (FilePath)
import qualified System.Directory as D
import System.FilePath
import System.PosixCompat.Files as PF

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path =
  ((map (path </>)) . filter (not . dots) <$> D.getDirectoryContents path) >>= filterM exists
  where
#if MIN_VERSION_directory(1, 2, 7)
    exists x = D.doesPathExist x
#else
    exists x = (||) <$> D.doesFileExist x <*> D.doesDirectoryExist x
#endif
    dots "."  = True
    dots ".." = True
    dots _    = False

fileDirContents :: FilePath -> IO ([FilePath], [FilePath])
fileDirContents path = do
  contents <- getDirectoryContentsPath path
  stats <- mapM getFileStatus contents
  let pairs = zip stats contents
  let files = [ f | (s, f) <- pairs, PF.isRegularFile s]
  let dirs = [ d | (s, d) <- pairs, PF.isDirectory s]
  return (files, dirs)

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  (files, dirs) <- fileDirContents path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

findImmediateFiles :: FilePath -> IO [FilePath]
findImmediateFiles = fileDirContents >=> mapM D.canonicalizePath . fst

-- * Exported functions below this point

findFiles :: Bool -> FilePath -> IO [FilePath]
findFiles True path  = findAllFiles       =<< canonicalizeDirPath path
findFiles False path = findImmediateFiles =<<  canonicalizeDirPath path

findFilesAndDirs :: Bool -> FilePath -> IO [FilePath]
findFilesAndDirs False path = getDirectoryContentsPath =<< canonicalizeDirPath path
findFilesAndDirs True path = do
  (files, dirs) <- fileDirContents path
  nestedFilesAndDirs <- concat <$> mapM (findFilesAndDirs False) dirs
  return (files ++ dirs ++ nestedFilesAndDirs)

-- | add a trailing slash to ensure the path indicates a directory
addTrailingSlash :: FilePath -> FilePath
addTrailingSlash = addTrailingPathSeparator

canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingSlash `fmap` D.canonicalizePath path

-- | bugfix older version of canonicalizePath (system-fileio <= 0.3.7) loses trailing slash
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = let was_dir = null (takeFileName path) in
  if not was_dir then D.canonicalizePath path
  else canonicalizeDirPath path

hasThisExtension :: FilePath -> T.Text -> Bool
hasThisExtension p ext = takeExtension p == T.unpack ext
