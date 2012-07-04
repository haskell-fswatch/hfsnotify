module Main where

import Prelude hiding (FilePath, writeFile)

import Data.ByteString (empty)
import Filesystem
import Filesystem.Path hiding (empty)
import Filesystem.Path.CurrentOS hiding (empty)
import System.IO.FSNotify.Types
import Util

data EventCounter = EventCounter {
    addedCount   :: Int
  , removedCount :: Int
  }

newCounter :: EventCounter
newCounter = EventCounter 0 0

testFile :: FilePath -> FilePath
testFile path = (path </> (decodeString "test.txt"))

write :: FilePath -> IO ()
write path = writeFile (testFile path) empty

delete :: FilePath -> IO ()
delete path = removeFile (testFile path)

action :: FilePath -> IO ()
action path = do
  write path
  delete path

verify :: EventProcessor
verify report@(TestReport _ events) = if addedCount counter == 1 && removedCount counter == 1 then
                                 TestResult True "" report
                               else
                                 TestResult False "Expected 1 Added, 1 Removed event in stream" report
  where
    counter = countEvents report newCounter

countEvents :: TestReport -> EventCounter -> EventCounter
countEvents (TestReport path ((Added eventPath):events)) (EventCounter added removed)
  | path == eventPath = countEvents (TestReport path events) (EventCounter (added + 1) removed)
  | otherwise         = countEvents (TestReport path events) (EventCounter added removed)
countEvents (TestReport path ((Removed eventPath):events)) (EventCounter added removed)
  | path == eventPath = countEvents (TestReport path events) (EventCounter added (removed + 1))
  | otherwise         = countEvents (TestReport path events) (EventCounter added removed)
countEvents (TestReport path (_:events)) (EventCounter added removed) =
                        countEvents (TestReport path events) (EventCounter added removed)
countEvents (TestReport _ _) counter = counter

main :: IO ()
main = inEnv ChanEnv DirEnv act action verify
