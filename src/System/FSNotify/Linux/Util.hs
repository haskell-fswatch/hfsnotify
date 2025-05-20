{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module System.FSNotify.Linux.Util (
  canonicalizePath
  , canonicalizeRawDirPath
  , (<//>)
  , traverseAllDirs

  , boolToIsDirectory

  , fromRawFilePath
  , toRawFilePath

  , fromHinotifyPath

  , rawToHinotifyPath
  , rawFromHinotifyPath
  ) where

import Control.Exception.Safe as E
import Control.Monad
import qualified Data.ByteString as BS
import Data.Function
import Data.Monoid
import qualified GHC.Foreign as F
import GHC.IO.Encoding (getFileSystemEncoding)
import Prelude hiding (FilePath)
import System.Directory (canonicalizePath)
import System.FSNotify.Types
import System.FilePath (FilePath)
import System.Posix.ByteString (RawFilePath)
import System.Posix.Directory.ByteString (openDirStream, readDirStream, closeDirStream)
import System.Posix.Files (getSymbolicLinkStatus, isDirectory)


canonicalizeRawDirPath :: RawFilePath -> IO RawFilePath
canonicalizeRawDirPath p = fromRawFilePath p >>= canonicalizePath >>= toRawFilePath

-- | Same as </> but for RawFilePath
-- TODO: make sure this is correct or find in a library
(<//>) :: RawFilePath -> RawFilePath -> RawFilePath
x <//> y = x <> "/" <> y

traverseAllDirs :: RawFilePath -> (RawFilePath -> IO ()) -> IO ()
traverseAllDirs dir cb = traverseAll dir $ \subPath ->
  -- TODO: wish we didn't need fromRawFilePath here
  -- TODO: should this follow symlinks? (What then about symlinks that escape the parent?)
  fromRawFilePath subPath >>= getSymbolicLinkStatus >>= \case
    (isDirectory -> True) -> cb subPath >> return True
    _ -> return False

traverseAll :: RawFilePath -> (RawFilePath -> IO Bool) -> IO ()
traverseAll dir cb = bracket (openDirStream dir) closeDirStream $ \dirStream ->
  fix $ \loop -> do
    readDirStream dirStream >>= \case
      x | BS.null x -> return ()
      "." -> loop
      ".." -> loop
      subDir -> flip finally loop $ do
        -- TODO: canonicalize?
        let fullSubDir = dir <//> subDir
        shouldRecurse <- cb fullSubDir
        when shouldRecurse $ traverseAll fullSubDir cb

boolToIsDirectory :: Bool -> EventIsDirectory
boolToIsDirectory False = IsFile
boolToIsDirectory True = IsDirectory

toRawFilePath :: FilePath -> IO BS.ByteString
toRawFilePath fp = do
  enc <- getFileSystemEncoding
  F.withCString enc fp BS.packCString

fromRawFilePath :: BS.ByteString -> IO FilePath
fromRawFilePath bs = do
  enc <- getFileSystemEncoding
  BS.useAsCString bs (F.peekCString enc)

#if MIN_VERSION_hinotify(0, 3, 10)
fromHinotifyPath :: BS.ByteString -> IO FilePath
fromHinotifyPath = fromRawFilePath

rawToHinotifyPath :: BS.ByteString -> IO BS.ByteString
rawToHinotifyPath = return

rawFromHinotifyPath :: BS.ByteString -> IO BS.ByteString
rawFromHinotifyPath = return
#else
fromHinotifyPath :: FilePath -> IO FilePath
fromHinotifyPath = return

rawToHinotifyPath :: BS.ByteString -> IO FilePath
rawToHinotifyPath = fromRawFilePath

rawFromHinotifyPath :: FilePath -> IO BS.ByteString
rawFromHinotifyPath = toRawFilePath
#endif
