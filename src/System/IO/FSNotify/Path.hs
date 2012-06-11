--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module System.IO.FSNotify.Path
       ( fp
       , findFiles
       , findDirs
       ) where

import Prelude hiding (FilePath)

import Control.Monad
import Filesystem
import Filesystem.Path hiding (concat)
import Filesystem.Path.CurrentOS hiding (concat)

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
getDirectoryContentsPath path = listDirectory path >>= return . map (path </>)

findImmediateFiles :: FilePath -> IO [FilePath]
findImmediateFiles path = getDirectoryContentsPath path >>= filterM isFile

findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateDirs path = getDirectoryContentsPath path >>= filterM isDirectory

findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  files <- findImmediateFiles path
  dirs  <- findImmediateDirs  path
  nestedFiles <- mapM findAllFiles dirs
  return (files ++ concat nestedFiles)

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs  <- findImmediateDirs path
  nestedDirs <- mapM findAllDirs dirs
  return (dirs ++ concat nestedDirs)

findFiles :: Bool -> FilePath -> IO [FilePath]
findFiles True path  = findAllFiles       path
findFiles False path = findImmediateFiles path

findDirs :: Bool -> FilePath -> IO [FilePath]
findDirs True path  = findAllDirs       path
findDirs False path = findImmediateDirs path
