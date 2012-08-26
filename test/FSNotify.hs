module FSNotify (spec) where

import Prelude hiding (FilePath, writeFile)

import Control.Concurrent (threadDelay)
import Data.ByteString (empty)
import Filesystem (removeFile, rename, writeFile)
import Filesystem.Path.CurrentOS ((</>), FilePath)
import System.FilePath.Glob (compile, match, Pattern)
import System.IO.FSNotify.Path (fp)
import System.IO.FSNotify.Types
import Test.Hspec (describe, it, Spec)
import Test.HUnit.Lang (Assertion)
import Util

spec :: Spec
spec = do
  describe "watchDir" $ do
    it "Create file" $ testFileName "txt" >>= createFileSpec ActionEnv
    it "Remove file" $ testFileName "txt" >>= removeFileSpec ActionEnv
    it "Rename file" $ renameInput        >>= renameFileSpec ActionEnv
  describe "watchDir" $ do
    it "Create file" $ testFileName "txt" >>= createFileSpec ActionEnv
    it "Remove file" $ testFileName "txt" >>= removeFileSpec ActionEnv
    it "Rename file" $ renameInput        >>= renameFileSpec ActionEnv
  describe "watchTree" $ do
    it "Create file" $ testFileName "txt" >>= createFileSpecR ActionEnv
    it "Remove file" $ testFileName "txt" >>= removeFileSpecR ActionEnv
    it "Rename file" $ renameInput        >>= renameFileSpecR ActionEnv

createFileSpec :: ChanActionEnv -> FilePath -> Assertion
createFileSpec envType fileName = do
  inEnv envType DirEnv act action $ matchEvents matchers
  where
    action :: FilePath -> IO ()
    action envDir = writeFile (envDir </> fileName) empty
    matchers :: [EventPredicate]
    matchers = [EventPredicate "File creation" (matchCreate fileName)]

removeFileSpec :: ChanActionEnv -> FilePath -> Assertion
removeFileSpec envType fileName = do
  withTempDir $ \envDir -> do
    writeFile (envDir </> fileName) empty
    inTempDirEnv envType DirEnv act action (matchEvents matchers) envDir
  where
    action :: FilePath -> IO ()
    action envDir = removeFile (envDir </> fileName)
    matchers :: [EventPredicate]
    matchers = [EventPredicate "File deletion" (matchRemove fileName)]

renameFileSpec :: ChanActionEnv -> (FilePath, FilePath) -> Assertion
renameFileSpec envType (oldFileName, newFileName) = do
  withTempDir $ \envDir -> do
    writeFile (envDir </> oldFileName) empty
    inTempDirEnv envType DirEnv act action (matchEvents matchers) envDir
  where
    action :: FilePath -> IO ()
    action envDir = rename (envDir </> oldFileName) (envDir </> newFileName)
    matchers :: [EventPredicate]
    matchers = [ EventPredicate "Rename: File deletion" (matchRemove oldFileName)
               , EventPredicate "Rename: File creation" (matchCreate newFileName) ]

createFileSpecR :: ChanActionEnv -> FilePath -> Assertion
createFileSpecR envType fileName = do
  withTempDir $ \envDir -> do
    withNestedTempDir envDir $ \envPath -> do
      inTempDirEnv envType TreeEnv act (action envPath) (matchEvents matchers) envDir
  where
    action :: FilePath -> FilePath -> IO ()
    action envPath _ = do
      writeFile (envPath </> fileName) empty
    matchers :: [EventPredicate]
    matchers = [EventPredicate "File creation" (matchCreate fileName)]

removeFileSpecR :: ChanActionEnv -> FilePath -> Assertion
removeFileSpecR envType fileName = do
  withTempDir $ \envDir -> do
    withNestedTempDir envDir $ \envPath -> do
      writeFile (envPath </> fileName) empty
      inTempDirEnv envType TreeEnv act (action envPath) (matchEvents matchers) envDir
  where
    action :: FilePath -> FilePath -> IO ()
    action envPath _ = do
      removeFile (envPath </> fileName)
    matchers :: [EventPredicate]
    matchers = [EventPredicate "File deletion" (matchRemove fileName)]

renameFileSpecR :: ChanActionEnv -> (FilePath, FilePath) -> Assertion
renameFileSpecR envType (oldFileName, newFileName) = do
  withTempDir $ \envDir -> do
    withNestedTempDir envDir $ \envPath -> do
      writeFile (envPath </> oldFileName) empty
      inTempDirEnv envType TreeEnv act (action envPath) (matchEvents matchers) envDir
  where
    action :: FilePath -> FilePath -> IO ()
    action envPath _ = rename (envPath </> oldFileName) (envPath </> newFileName)
    matchers :: [EventPredicate]
    matchers = [ EventPredicate "Rename: File deletion" (matchRemove oldFileName)
               , EventPredicate "Rename: File creation" (matchCreate newFileName) ]

renameInput :: IO (FilePath, FilePath)
renameInput = do
  oldName <- testFileName "txt"
  newName <- testFileName "txt"
  return (oldName, newName)

matchCreate :: FilePath -> Event -> Bool
matchCreate fileName (Added path _) = matchFP pattern path
  where
    pattern = compile $  "**/*" ++ fp fileName
matchCreate _ _ = False

matchRemove :: FilePath -> Event -> Bool
matchRemove fileName (Removed path _) = matchFP pattern path
  where
    pattern = compile $  "**/*" ++ fp fileName
matchRemove _ _ = False

matchFP :: Pattern -> FilePath -> Bool
matchFP pattern path = match pattern $ fp path
