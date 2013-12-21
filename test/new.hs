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
      [ testGroup "Non-recursive"
        [ tNewFile
        , tModFile
        , tDelFile
        , tCreateRmDir
        , tNonRec
        ]
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

tCreateRmDir = testCase "directories are ignored" $ do
  let dir = testDirPath </> "tCreateRmDir"
  expectEventsHere [] $ do
    createDirectory (encodeString dir)
    removeDirectory (encodeString dir)

tNonRec = testCase "doesn't watch recursively" $ do
  let dir = testDirPath </> "tNonRecDir"
      fInDir = dir </> "file"
  createDirectory (encodeString dir)
  expectEventsHere [] $ do
    writeFile  (encodeString fInDir) "foo"
    removeFile (encodeString fInDir)
