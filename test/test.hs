{-# LANGUAGE OverloadedStrings, ImplicitParams, ViewPatterns #-}
import Prelude hiding
  ( FilePath, writeFile, writeFile )
import Test.Tasty
import Test.Tasty.HUnit
import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.FSNotify
import System.IO.Temp
import Text.Printf
import Control.Monad
import Control.Exception
import Control.Concurrent

import EventUtils

nativeMgrSupported :: IO Bool
nativeMgrSupported = do
  mgr <- startManager
  stopManager mgr
  return $ not $ isPollingManager mgr

main = do
  hasNative <- nativeMgrSupported
  unless hasNative $
    putStrLn "WARNING: native manager cannot be used or tested on this platform"
  defaultMain $
    withResource
      (createTree testDirPath)
      (const $ removeTree testDirPath) $
      const $ tests hasNative

tests hasNative = testGroup "Tests" $ do
  poll <-
    if hasNative
      then [False, True]
      else [True]
  let ?timeInterval =
        if poll
          then 2*10^6
          else 5*10^5
  return $ testGroup (if poll then "Polling" else "Native") $ do
  recursive <- [False, True]
  return $ testGroup (if recursive then "Recursive" else "Non-recursive") $ do
  nested <- [False, True]
  return $ testGroup (if nested then "In a subdirectory" else "Right here") $ do
  t <-
    [ mkTest "new file"
        (if poll then [evAdded] else [evAdded, evModified])
        (const $ return ())
        (\f -> writeFile f "foo")
    , mkTest "modify file" [evModified] (\f -> writeFile f "")
        (\f -> when poll (threadDelay $ 10^6) >> writeFile f "foo")
    , mkTest "delete file" [evRemoved] (\f -> writeFile f "") (\f -> removeFile f)
    , mkTest "directories are ignored" [] (const $ return ())
        (\f -> createDirectory False f >> removeDirectory f)
    ]
  return $ t nested recursive poll
  where
    mkTest title evs prepare action nested recursive poll =
      testCase title $
        withTempDirectory (encodeString testDirPath) "test." $ \(decodeString -> watchedDir) -> do
        let baseDir = if nested then watchedDir </> "subdir" else watchedDir
            f = baseDir </> filename
            expect =
              expectEvents poll
                (if recursive then watchTree else watchDir)
                watchedDir
        createDirectory True baseDir
        (prepare f >>
         expect (if not nested || recursive then map ($ f) evs else []) (action f))
          `finally` (isFile f >>= \b -> when b (removeFile f))

    filename = "testfile"
