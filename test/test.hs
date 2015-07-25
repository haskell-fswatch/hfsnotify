{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
import Prelude hiding
  ( FilePath )
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.FilePath
import System.FSNotify
import System.IO.Error
import System.IO.Temp
import System.PosixCompat.Files
import Control.Monad
import Control.Exception
import Control.Concurrent

import EventUtils

nativeMgrSupported :: IO Bool
nativeMgrSupported = do
  mgr <- startManager
  stopManager mgr
  return $ not $ isPollingManager mgr

main :: IO ()
main = do
  hasNative <- nativeMgrSupported
  unless hasNative $
    putStrLn "WARNING: native manager cannot be used or tested on this platform"
  defaultMain $
    withResource
      (createDirectoryIfMissing True testDirPath)
      (const $ removeDirectoryRecursive testDirPath) $
      const $ tests hasNative

tests :: Bool -> TestTree
tests hasNative = testGroup "Tests" $ do
  poll <-
    if hasNative
      then [False, True]
      else [True]
  let ?timeInterval =
        if poll
          then 2*10^(6 :: Int)
          else 5*10^(5 :: Int)
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
        (\f -> when poll (threadDelay $ 10^(6 :: Int)) >> writeFile f "foo")
    , mkTest "delete file" [evRemoved] (\f -> writeFile f "") (\f -> removeFile f)
    , mkTest "directories are ignored" [] (const $ return ())
        (\f -> createDirectory f >> removeDirectory f)
    ]
  return $ t nested recursive poll
  where
    mkTest title evs prepare action nested recursive poll =
      testCase title $
        withTempDirectory testDirPath "test." $ \watchedDir -> do
        let baseDir = if nested then watchedDir </> "subdir" else watchedDir
            f = baseDir </> fileName
            expect =
              expectEvents poll
                (if recursive then watchTree else watchDir)
                watchedDir
        createDirectoryIfMissing True baseDir
        (prepare f >>
         expect (if not nested || recursive then map ($ f) evs else []) (action f))
          `finally` (isFile f >>= \b -> when b (removeFile f))

    fileName = "testfile"


-------------------------------------------------------------------------------
isFile :: FilePath -> IO Bool
isFile p = handleJust h return checkFile
  where
    h e = if isDoesNotExistError e
          then Just False
          else Nothing
    checkFile = isRegularFile <$> getFileStatus p
