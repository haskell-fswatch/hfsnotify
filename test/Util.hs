module Util where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Exception
import Filesystem.Path.CurrentOS
import System.Directory
import System.Environment
import System.Exit
import System.IO.FSNotify
import System.IO.FSNotify.Types

data ChanActionEnv =
    ChanEnv
  | ActionEnv
data DirTreeEnv =
    DirEnv
  | TreeEnv
data TestContext = TestContext ChanActionEnv DirTreeEnv ActionPredicate

data TestReport = TestReport FilePath [Event] deriving (Show)
data TestResult = TestResult Bool String TestReport
type TestAction = FilePath -> IO ()
type EventProcessor = TestReport -> TestResult

testName :: IO String
testName = do
    n <- getProgName
    return (n ++ "-sandbox")

withTempDir :: (String -> IO ()) -> IO ()
withTempDir fn = do
    path <- testName
    bracket (createDirectory path >> return path) removeDirectoryRecursive fn

actAndReport :: TestAction -> FilePath -> EventChannel -> EventProcessor -> IO TestResult
actAndReport action path chan processor = do
  _      <- action path
  events <- getChanContents chan
  return $ processor (TestReport path events)

withWatchManager :: (WatchManager -> IO ()) -> IO ()
withWatchManager action = bracket startManager action stopManager

inEnv :: ChanActionEnv -> DirTreeEnv -> ActionPredicate -> TestAction -> EventProcessor -> IO ()
inEnv caEnv dtEnv reportPred action eventProcessor = withTempDir $ \pathString ->
  withWatchManager $ \manager -> do
    chan <- newChan
    let path = decodeString pathString
    _ <- watchInEnv caEnv dtEnv manager path reportPred chan
    actAndReport action path chan eventProcessor >>= exitStatus

actionAsChan :: (WatchManager -> FilePath -> ActionPredicate -> Action       -> IO ()) ->
                 WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
actionAsChan actionFunction wm fp ap ec = actionFunction wm fp ap (writeChan ec)

watchInEnv ChanEnv   DirEnv  = watchDirChan
watchInEnv ChanEnv   TreeEnv = watchTreeChan
watchInEnv ActionEnv DirEnv  = actionAsChan watchDirAction
watchInEnv ActionEnv TreeEnv = actionAsChan watchTreeAction

exitStatus :: TestResult -> IO ()
exitStatus (TestResult True  _ _)  = testSuccess
exitStatus (TestResult False explanation report) = explainFailure explanation report >> testFailure

explainFailure :: String -> TestReport -> IO ()
explainFailure explanation report = do
    putStrLn explanation
    putStrLn $ "Events: " ++ show report

testFailure :: IO ()
testFailure = exitFailure

testSuccess :: IO ()
testSuccess = exitWith ExitSuccess
