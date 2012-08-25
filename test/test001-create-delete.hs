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

testFileName :: FilePath
testFileName = decodeString "test.txt"

testFile :: FilePath -> FilePath
testFile path = (path </> testFileName)

write :: FilePath -> IO ()
write path = writeFile (testFile path) empty

delete :: FilePath -> IO ()
delete path = removeFile (testFile path)

action :: FilePath -> IO ()
action path = do
  write path
  delete path

expectation :: String
expectation = "1 Added, 1 Removed event in stream"

verify :: EventProcessor
verify report@(TestReport _ events) =
  if addedCount counter == 1 && removedCount counter == 1 then
    return (TestResult True ("Found " ++ expectation) report)
    else
    return (TestResult False ("Expected " ++ expectation) report)
  where
    counter = countEvents report

countEvents :: TestReport -> EventCounter
countEvents = countEvents' newCounter

countEvents' :: EventCounter -> TestReport -> EventCounter
countEvents' (EventCounter added removed) (TestReport path ((Added eventPath _):events))
  | filename eventPath == testFileName = countEvents' (EventCounter (added + 1)  removed) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added       removed) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path ((Removed eventPath _):events))
  | filename eventPath == testFileName = countEvents' (EventCounter  added (removed + 1)) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path (_:events)) =
                                countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' counter _ = counter

main :: IO ()
main = inEnv ActionEnv DirEnv act action verify
