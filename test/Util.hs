module Util where

import Prelude hiding (FilePath, catch)

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (when)
import Data.Unique.Id
import Filesystem.Path.CurrentOS
import System.Directory
import System.Environment
import System.Exit
import System.IO.Error (isPermissionError)
import System.IO.FSNotify
import System.IO.FSNotify.Types
import System.Random

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

testName :: IO String
testName = do
  id <- randomIO >>= initIdSupply >>= return . show . hashedId . idFromSupply
  return $ encodeString (decodeString ("sandbox-" ++ id) </> empty)

withTempDir :: (String -> IO ()) -> IO ()
withTempDir fn = do
  -- idSupply <- randomIO >>= initIdSupply
  path <- testName
  bracket (createDirectory path >> return path) attemptDirectoryRemoval fn
  where
    attemptDirectoryRemoval :: String -> IO ()
    attemptDirectoryRemoval path = catch
        (removeDirectoryRecursive path)
        (\e -> when
               (not $ isPermissionError e)
               (throw e))

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

inEnv :: ChanActionEnv -> DirTreeEnv -> ActionPredicate -> TestAction -> EventProcessor -> IO ()
inEnv caEnv dtEnv reportPred action eventProcessor = withTempDir $ \pathString ->
  withManager $ \manager -> do
    chan <- newChan
    let path = decodeString pathString
    watchInEnv caEnv dtEnv manager path reportPred chan
    actAndReport action path chan eventProcessor >>= explainResult >>= exitStatus

actionAsChan :: (WatchManager -> FilePath -> ActionPredicate -> Action       -> IO ()) ->
                 WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
actionAsChan actionFunction wm fp ap ec = actionFunction wm fp ap (writeChan ec)

watchInEnv ChanEnv   DirEnv  = watchDirChan
watchInEnv ChanEnv   TreeEnv = watchTreeChan
watchInEnv ActionEnv DirEnv  = actionAsChan watchDir
watchInEnv ActionEnv TreeEnv = actionAsChan watchTree

exitStatus :: Bool -> IO ()
exitStatus True  = testSuccess
exitStatus False = testFailure

explainResult :: TestResult -> IO Bool
explainResult (TestResult status explanation report) = do
  putStrLn ""
  if status then
     putStrLn " :: TEST SUCCESS"
     else
     putStrLn " !! TEST FAILURE"
  putStrLn ""
  putStrLn explanation
  putStrLn $ "Test report: " ++ show report
  return status

testFailure :: IO ()
testFailure = exitFailure

testSuccess :: IO ()
testSuccess = exitWith ExitSuccess
