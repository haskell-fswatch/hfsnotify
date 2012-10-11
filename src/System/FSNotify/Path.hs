--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module System.FSNotify.Path
       ( fp
       , findFiles
       , findDirs
       , canonicalizeDirPath
       , canonicalizePath
       ) where

import Prelude hiding (FilePath)

import Control.Monad
-- import Filesystem
-- import Filesystem.Path hiding (concat)
import Filesystem.Path.CurrentOS hiding (concat)

import qualified Filesystem as FS
import qualified Filesystem.Path as FP

-- This will ensure than any calls to fp for type coercion in FSNotify will not
-- break when/if the dependent package moves from using String to the more
-- efficient Filesystem.Path.FilePath
class ConvertFilePath a b where
  fp :: a -> b
instance ConvertFilePath FilePath String where fp   = encodeString
instance ConvertFilePath String FilePath where fp   = decodeString
instance ConvertFilePath String String where fp     = id
instance ConvertFilePath FilePath FilePath where fp = id

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path = fmap (map (path </>)) $ FS.listDirectory path

fileDirContents :: FilePath -> IO ([FilePath],[FilePath])
fileDirContents path = do
  contents <- getDirectoryContentsPath path
  files <- filterM FS.isFile contents
  dirs <- filterM FS.isDirectory contents
  return (files, dirs)

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  (files, dirs) <- fileDirContents path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

findImmediateFiles, findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateFiles = getDirectoryContentsPath >=> filterM FS.isFile >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize files = mapM FS.canonicalizePath files
findImmediateDirs  = getDirectoryContentsPath >=> filterM FS.isDirectory >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize dirs = mapM canonicalizeDirPath dirs

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs <- findImmediateDirs path
  nestedDirs <- mapM findAllDirs dirs
  return (dirs ++ concat nestedDirs)

findFiles :: Bool -> FilePath -> IO [FilePath]
findFiles True path  = findAllFiles       =<< canonicalizeDirPath path
findFiles False path = findImmediateFiles =<<  canonicalizeDirPath path

findDirs :: Bool -> FilePath -> IO [FilePath]
findDirs True path  = findAllDirs       =<< canonicalizeDirPath path
findDirs False path = findImmediateDirs =<< canonicalizeDirPath path

-- | add a trailing slash to ensure the path indicates a directory
addTrailingSlash :: FilePath -> FilePath
addTrailingSlash p =
 if FP.null (FP.filename p) then p else
   p FP.</> FP.empty

canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingSlash `fmap` FS.canonicalizePath path

-- | bugfix older version of canonicalizePath (system-fileio <= 0.3.7) loses trailing slash
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = let was_dir = FP.null (FP.filename path) in
  if not was_dir then FS.canonicalizePath path
  else canonicalizeDirPath path
