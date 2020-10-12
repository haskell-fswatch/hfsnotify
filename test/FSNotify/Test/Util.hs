{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf, LambdaCase, RecordWildCards, ViewPatterns #-}
-- |

module FSNotify.Test.Util where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Retry
import Data.IORef
import System.Directory
import System.FilePath
import System.FSNotify
import System.IO.Temp
import System.PosixCompat.Files (touchFile)
import System.Random as R
import Test.Hspec
import Test.HUnit.Lang

#ifdef mingw32_HOST_OS
import Data.Bits
import System.Win32.File (getFileAttributes, setFileAttributes, fILE_ATTRIBUTE_TEMPORARY)
-- Perturb the file's attributes, to check that a modification event is emitted
changeFileAttributes :: FilePath -> IO ()
changeFileAttributes file = do
  attrs <- getFileAttributes file
  setFileAttributes file (attrs `xor` fILE_ATTRIBUTE_TEMPORARY)
#else
changeFileAttributes :: FilePath -> IO ()
changeFileAttributes = touchFile
#endif


isMac :: Bool
#ifdef darwin_HOST_OS
isMac = True
#else
isMac = False
#endif

isWin :: Bool
#ifdef mingw32_HOST_OS
isWin = True
#else
isWin = False
#endif

nativeMgrSupported :: IO Bool
nativeMgrSupported = do
  mgr <- startManager
  stopManager mgr
  return $ not $ isPollingManager mgr

pauseAndRetryOnExpectationFailure :: (?timeInterval :: Int) => Int -> IO a -> IO a
pauseAndRetryOnExpectationFailure n action = threadDelay ?timeInterval >> retryOnExpectationFailure n action

retryOnExpectationFailure :: Int -> IO a -> IO a
#if MIN_VERSION_retry(0, 7, 0)
retryOnExpectationFailure seconds action = recovering (constantDelay 50000 <> limitRetries (seconds * 20)) [\_ -> Handler handleFn] (\_ -> action)
#else
retryOnExpectationFailure seconds action = recovering (constantDelay 50000 <> limitRetries (seconds * 20)) [\_ -> Handler handleFn] (action)
#endif
  where
    handleFn :: SomeException -> IO Bool
    handleFn (fromException -> Just (HUnitFailure {})) = return True
    handleFn _ = return False


makeTestFolder :: (?timeInterval :: Int) => ThreadingMode -> Bool -> Bool -> Bool -> SpecWith (FilePath, FilePath, IO [Event], IO ()) -> Spec
makeTestFolder threadingMode poll recursive nested = around $ \action -> do
  withRandomTempDirectory $ \watchedDir -> do
    let fileName = "testfile"
    let baseDir = if nested then watchedDir </> "subdir" else watchedDir
    let watchFn = if recursive then watchTree else watchDir

    createDirectoryIfMissing True baseDir

    -- On Mac, delay before starting the watcher because otherwise creation of "subdir"
    -- can get picked up.
    when isMac $ threadDelay 2000000

    let conf = defaultConfig {
          confDebounce = NoDebounce
          , confWatchMode = if poll then WatchModePoll (2 * 10^(5 :: Int)) else WatchModeOS
          , confThreadingMode = threadingMode
          }

    withManagerConf conf $ \mgr -> do
      eventsVar <- newIORef []
      stop <- watchFn mgr watchedDir (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ())))
      let clearEvents = threadDelay ?timeInterval >> atomicWriteIORef eventsVar []
      _ <- action (watchedDir, normalise $ baseDir </> fileName, readIORef eventsVar, clearEvents)
      stop


-- | Use a random identifier so that every test happens in a different folder
-- This is unfortunately necessary because of the madness of OS X FSEvents; see the comments in OSX.hs
withRandomTempDirectory :: (FilePath -> IO ()) -> IO ()
withRandomTempDirectory action = do
  randomID <- replicateM 10 $ R.randomRIO ('a', 'z')
  withSystemTempDirectory ("test." <> randomID) action
