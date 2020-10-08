-- | Adapted from how Shelly does finding in Shelly.Find
-- (shelly is BSD-licensed)

module System.FSNotify.Find where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath
import System.Posix.Files

find :: Bool -> FilePath -> IO [FilePath]
find followSymlinks = find' followSymlinks  []

find' :: Bool -> [FilePath] -> FilePath -> IO [FilePath]
find' followSymlinks startValue dir = do
  (rPaths, aPaths) <- lsRelAbs dir
  foldM visit startValue (zip rPaths aPaths)
  where
    visit acc (relativePath, absolutePath) = do
      isDir <- liftIO $ doesDirectoryExist absolutePath
      sym <- liftIO $ fmap isSymbolicLink $ getSymbolicLinkStatus absolutePath
      let newAcc = relativePath : acc
      if isDir && (followSymlinks || not sym)
        then find' followSymlinks newAcc relativePath
        else return newAcc

-- * Util

absPath :: FilePath -> IO FilePath
absPath p | null p = liftIO $ throwIO $ userError "Empty path"
          | isRelative p = do
            cwd <- getCurrentDirectory
            return (cwd </> p)
          | otherwise = return p

lsRelAbs :: FilePath -> IO ([FilePath], [FilePath])
lsRelAbs f = absPath f >>= \fp -> do
  files <- liftIO $ listDirectory fp
  let absolute = map (fp </>) files
  let relativized = map (\p -> joinPath [f, p]) files
  return (relativized, absolute)
