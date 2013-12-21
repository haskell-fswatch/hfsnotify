{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Test.Tasty
import Test.Tasty.HUnit
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.Directory
import Text.Printf
import Control.Monad
import Control.Exception

import EventUtils

subdirPath = testDirPath </> "subdir"

main = defaultMain $
  withResource
    (createDirectoryIfMissing True (encodeString subdirPath))
    (const $ removeDirectoryRecursive $ encodeString testDirPath) $
    tests

tests = testGroup "Tests" $ do
  recursive <- [False, True]
  return $ testGroup (if recursive then "Recursive" else "Non-recursive") $ do
  nested <- [False, True]
  return $ testGroup (if nested then "In a subdirectory" else "Right here") $ do
  t <-
    [ mkTest "new file" [evAdded, evModified] (const $ return ()) (\f -> writeFile f "foo")
    , mkTest "modify file" [evModified] (\f -> writeFile f "") (\f -> writeFile f "foo")
    , mkTest "delete file" [evRemoved] (\f -> writeFile f "") (\f -> removeFile f)
    , mkTest "directories are ignored" [] (const $ return ())
        (\f -> createDirectory f >> removeDirectory f)
    ]
  return $ t nested recursive
  where
    mkTest title evs prepare action nested recursive =
      testCase title $ do
        let baseDir = if nested then subdirPath else testDirPath
            f = baseDir </> filename
            fStr = encodeString f
            expectEvents =
              if recursive
                then expectEventsHereRec
                else expectEventsHere
        (prepare fStr >>
         expectEvents (if not nested || recursive then map ($ f) evs else []) (action fStr))
          `finally` (doesFileExist fStr >>= \b -> when b (removeFile fStr))

    filename = "testfile"
