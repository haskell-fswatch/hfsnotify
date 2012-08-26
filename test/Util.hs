module Util where

import Prelude hiding (FilePath, catch)

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (when)
import Data.Unique.Id
import Filesystem.Path.CurrentOS hiding (concat)
import System.Directory
import System.Environment
import System.Exit
import System.IO.Error (isPermissionError)
import System.IO.FSNotify
import System.IO.FSNotify.Path
import System.IO.FSNotify.Types
import System.Random
import System.Timeout (timeout)

data ChanActionEnv =
    ChanEnv
  | ActionEnv
data DirTreeEnv =
    DirEnv
  | TreeEnv
data TestContext = TestContext ChanActionEnv DirTreeEnv ActionPredicate

data TestReport = TestReport FilePath [Event] deriving (Show)
data TestResult = TestResult Bool String TestReport deriving (Show)
type TestAction = FilePath -> IO ()
type EventProcessor = TestReport -> IO TestResult
data EventPredicate = EventPredicate String (Event -> Bool)

predicateName :: EventPredicate -> String
predicateName (EventPredicate name _) = name

matchEvents :: [EventPredicate] -> EventProcessor
matchEvents preds@((EventPredicate _ pred):restPreds) (TestReport path (event:events))
  | pred event = matchEvents restPreds (TestReport path events)
  | otherwise  = matchEvents preds    (TestReport path events)
matchEvents preds@(pred:resPreds) report@(TestReport path []) =
  return (TestResult False (concat $ map predicateName preds) report)
matchEvents [] report = return (TestResult True "" report)

newId :: IO String
newId = randomIO >>= initIdSupply >>= return . show . hashedId . idFromSupply

testFileName :: String -> IO FilePath
testFileName ext = do
  uId <- newId
  return $ fp ("test-" ++ uId ++ "." ++ ext)

testName :: IO FilePath
testName = do
  uId <- newId
  return $ fp ("sandbox-" ++ uId) </> empty

withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir fn = withNestedTempDir empty fn

withNestedTempDir :: FilePath -> (FilePath -> IO ()) -> IO ()
withNestedTempDir firstPath fn = do
  secondPath <- testName
  let path = if firstPath /= empty then
               fp $ firstPath </> secondPath
             else
               fp secondPath
  bracket (createDirectory path >> return path) (attemptDirectoryRemoval . fp) (fn . fp)

attemptDirectoryRemoval :: FilePath -> IO ()
attemptDirectoryRemoval path = catch
                               (removeDirectoryRecursive pathString)
                               (\e -> when
                                      (not $ isPermissionError e)
                                      (throw e))
  where
    pathString = fp path

performAction :: TestAction -> FilePath -> IO ()
performAction action path = action path

reportOnAction :: FilePath -> EventChannel -> EventProcessor -> IO TestResult
reportOnAction = reportOnAction' []

reportOnAction' :: [Event] -> FilePath -> EventChannel -> EventProcessor -> IO TestResult
reportOnAction' events path chan processor = do
  result@(TestResult status _ _) <- processor (TestReport path events)
  if not status then do
    event <- readChan chan
    reportOnAction' (event:events) path chan processor
    else
    return result

actAndReport :: TestAction -> FilePath -> EventChannel -> EventProcessor -> IO TestResult
actAndReport action path chan processor = do
  performAction action path
  reportOnAction path chan processor

testTimeout :: Int
testTimeout = 3000000

timeoutTest :: Maybe () -> IO ()
timeoutTest Nothing = error "Test timed out"
timeoutTest (Just _) = return ()

inEnv :: ChanActionEnv -> DirTreeEnv -> ActionPredicate -> TestAction -> EventProcessor -> IO ()
inEnv caEnv dtEnv reportPred action eventProcessor =
  withTempDir $ inTempDirEnv caEnv dtEnv reportPred action eventProcessor

runTest :: IO () -> IO ()
runTest test = timeout testTimeout test >>= timeoutTest

inTempDirEnv :: ChanActionEnv -> DirTreeEnv -> ActionPredicate -> TestAction -> EventProcessor-> FilePath -> IO ()
inTempDirEnv caEnv dtEnv reportPred action eventProcessor path =
  runTest $ do
    withManager $ \manager -> do
      chan <- newChan
      watchInEnv caEnv dtEnv manager path reportPred chan
      actAndReport action path chan eventProcessor >>= outputOnFail

actionAsChan :: (WatchManager -> FilePath -> ActionPredicate -> Action       -> IO ()) ->
                 WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
actionAsChan actionFunction wm fp ap ec = actionFunction wm fp ap (writeChan ec)

watchInEnv :: ChanActionEnv
           -> DirTreeEnv
           -> WatchManager
           -> FilePath
           -> ActionPredicate
           -> EventChannel
           -> IO ()
watchInEnv ChanEnv   DirEnv  = watchDirChan
watchInEnv ChanEnv   TreeEnv = watchTreeChan
watchInEnv ActionEnv DirEnv  = actionAsChan watchDir
watchInEnv ActionEnv TreeEnv = actionAsChan watchTree

outputOnFail :: TestResult -> IO ()
outputOnFail (TestResult False explanation report) = do
  error $ show explanation ++ " " ++ show report
outputOnFail (TestResult True _ _) = return ()
