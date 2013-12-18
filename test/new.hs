{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Concurrent.Async
import Control.Applicative
import Control.Monad
import Data.IORef
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.Directory
import System.FSNotify
import System.IO hiding (FilePath)
import System.IO.Unsafe
import Text.Printf

-- Time to wait for events, microseconds
timeInterval :: Int
timeInterval = 10^5

delay :: IO ()
delay = threadDelay timeInterval

testDirPath :: FilePath
testDirPath = decodeString (unsafePerformIO getCurrentDirectory) </> "testdir"

-- event patterns
data EventPattern = EventPattern
  { patFile :: FilePath
  , patName :: String
  , patPredicate :: Event -> Bool
  }

evAdded, evRemoved, evModified :: FilePath -> EventPattern
evAdded path =
  EventPattern
    path
    "Added"
    (\x -> case x of Added path' _ -> path == path'; _ -> False)
evRemoved path =
  EventPattern
    path
    "Removed"
    (\x -> case x of Removed path' _ -> path == path'; _ -> False)
evModified path =
  EventPattern
    path
    "Modified"
    (\x -> case x of Modified path' _ -> path == path'; _ -> False)

matchEvents :: [EventPattern] -> [Event] -> Assertion
matchEvents expected actual = do
  unless (length expected == length actual) $
    assertFailure $ printf
      "Unexpected number of events.\n  Expected: %s\n  Actual: %s\n"
      (show expected)
      (show actual)
  sequence_ $ (\f -> zipWith f expected actual) $ \pat ev ->
    assertBool
      (printf "Unexpected event.\n  Expected :%s\n  Actual: %s\n"
        (show expected)
        (show actual))
      (patPredicate pat ev)

instance Show EventPattern where
  show p = printf "%s %s" (patName p) (show $ patFile p)

gatherEvents
  :: (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
     -- (^ this is the type of watchDir/watchTree)
  -> FilePath
  -> IO (Async [Event])
gatherEvents watch path = do
  mgr <- startManagerConf NoDebounce
  eventsVar <- newIORef []
  stop <- watch mgr path (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ())))
  async $ do
    delay
    stop
    reverse <$> readIORef eventsVar

expectEvents
  :: (WatchManager -> FilePath -> ActionPredicate -> Action -> IO StopListening)
  -> FilePath -> [EventPattern] -> IO () -> Assertion
expectEvents w path pats action = do
  a <- gatherEvents w path
  action
  evs <- wait a
  matchEvents pats evs

expectEventsHere = expectEvents watchDir testDirPath
expectEventsHereRec = expectEvents watchTree testDirPath

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
