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
  } deriving (Show)

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

verify :: EventProcessor
verify report@(TestReport _ events) = do
  putStrLn "Performing event verification"
  putStrLn $ "Event counts: " ++ show counter
  res <- return (if addedCount counter == 1 && removedCount counter == 1 then
    TestResult True "" report
    else
    TestResult False "Expected 1 Added, 1 Removed event in stream" report)
  putStrLn $ "Verification complete: " ++ (show res)
  return res
  where
    counter = countEvents report

countEvents :: TestReport -> EventCounter
countEvents = countEvents' newCounter

countEvents' :: EventCounter -> TestReport -> EventCounter
countEvents' (EventCounter added removed) (TestReport path ((Added eventPath):events))
  | eventPath == testFileName = countEvents' (EventCounter (added + 1)  removed) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added       removed) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path ((Removed eventPath):events))
  | eventPath == testFileName = countEvents' (EventCounter  added (removed + 1)) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path (_:events)) =
                                countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' counter _ = counter

-- countEvents (TestReport path ((Added   eventPath):events)) (EventCounter added removed)
-- countEvents (TestReport path ((Removed eventPath):events)) (EventCounter added removed)
-- countEvents (TestReport path ((_                ):events)) (EventCounter added removed)
-- countEvents (TestReport _      _                         )  counter


main :: IO ()
main = inEnv ActionEnv DirEnv act action verify
