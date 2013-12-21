{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Test.Tasty
import Test.Tasty.HUnit
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.Directory

import EventUtils

main = defaultMain $
  withResource
    (createDirectory $ encodeString testDirPath)
    (const $ removeDirectoryRecursive $ encodeString testDirPath) $
    testGroup "Tests"
      [ tNewFile
      , tModFile
      , tDelFile
      ]

tNewFile = testCase "new file" $ do
  let f = testDirPath </> "tNewFile"
  expectEventsHere [evAdded f, evModified f] $
    writeFile (encodeString f) "foo"

tModFile = testCase "modify file" $ do
  let f = testDirPath </> "tModFile"
  writeFile (encodeString f) ""
  delay
  expectEventsHere [evModified f] $
    writeFile (encodeString f) "foo"

tDelFile = testCase "remove file" $ do
  let f = testDirPath </> "tDelFile"
  writeFile (encodeString f) ""
  delay
  expectEventsHere [evRemoved f] $
    removeFile (encodeString f)
