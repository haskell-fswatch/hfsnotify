{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FSNotify.Test.Util where

import Control.Exception.Safe (Handler(..))
import Control.Monad.Logger
import Control.Retry
import Data.String.Interpolate
import System.FSNotify
import System.FilePath
import Test.Sandwich
import UnliftIO hiding (poll, Handler)
import UnliftIO.Concurrent
import UnliftIO.Directory

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

#ifdef mingw32_HOST_OS
import Data.Bits
import System.Win32.File (getFileAttributes, setFileAttributes, fILE_ATTRIBUTE_TEMPORARY)

-- Perturb the file's attributes, to check that a modification event is emitted
changeFileAttributes :: FilePath -> IO ()
changeFileAttributes file = do
  attrs <- getFileAttributes file
  setFileAttributes file (attrs `xor` fILE_ATTRIBUTE_TEMPORARY)
#else
import System.PosixCompat.Files (touchFile)

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

isLinux :: Bool
#ifdef linux_HOST_OS
isLinux = True
#else
isLinux = False
#endif

isBSD :: Bool
#ifdef OS_BSD
isBSD = True
#else
isBSD = False
#endif

waitUntil :: MonadUnliftIO m => Double -> m a -> m a
#if MIN_VERSION_retry(0, 7, 0)
waitUntil timeInSeconds action = withRunInIO $ \runInIO ->
  recovering policy [\_ -> Handler handleFn] (\_ -> runInIO action)
#else
waitUntil timeInSeconds action = withRunInIO $ \runInIO ->
  recovering policy [\_ -> Handler handleFn] (runInIO action)
#endif
  where
    handleFn :: SomeException -> IO Bool
    handleFn (fromException -> Just (_ :: FailureReason)) = return True
    handleFn _ = return False

    policy = limitRetriesByCumulativeDelay (round (timeInSeconds * 1000000.0)) $ capDelay 1000000 $ exponentialBackoff 1000


data TestFolderContext = TestFolderContext {
  watchedDir :: FilePath
  , filePath :: FilePath
  , getEvents :: IO [Event]
  , clearEvents :: IO ()
  }

data TestFolderGenerator = TestFolderGenerator {
  testFolderGeneratorRootDir :: FilePath
  , testFolderGeneratorId :: MVar Int
  }

newTestFolderGenerator :: MonadUnliftIO m => FilePath -> m TestFolderGenerator
newTestFolderGenerator dir = TestFolderGenerator dir <$> newMVar 0

withTestFolderGenerator :: MonadUnliftIO m => (TestFolderGenerator -> m a) -> m a
withTestFolderGenerator action = do
  withSystemTempDirectory "hfsnotify-tests" $ \dir ->
    newTestFolderGenerator dir >>= action

withRandomTempDirectory :: MonadUnliftIO m => TestFolderGenerator -> (FilePath -> m a) -> m a
withRandomTempDirectory (TestFolderGenerator {..}) action = do
  testId <- modifyMVar testFolderGeneratorId $ \x ->
    return (x + 1, x)
  let dir = testFolderGeneratorRootDir </> ("test_" <> show testId)
  bracket_ (createDirectory dir)
           (removePathForcibly dir)
           (action dir)

withTestFolder :: (
  MonadUnliftIO m, MonadLogger m
  )
  => TestFolderGenerator
  -> ThreadingMode
  -> Bool
  -> Bool
  -> Bool
  -> (FilePath -> m ())
  -> (TestFolderContext -> m a)
  -> m a
withTestFolder testFolderGenerator threadingMode poll recursive nested setup action = do
  withRandomTempDirectory testFolderGenerator $ \watchedDir' -> do
    info [i|Got temp directory: #{watchedDir'}|]
    let fileName = "testfile"
    let baseDir = if nested then watchedDir' </> "subdir" else watchedDir'
    let watchFn = if recursive then watchTree else watchDir

    createDirectoryIfMissing True baseDir

    let p = normalise $ baseDir </> fileName

    setup p

    let pollInterval = 2 * 10^(5 :: Int)

    -- Delay before starting the watcher to make sure setup events picked up.
    --
    -- For MacOS, we can apparently get an event for the creation of "subdir" when doing nested tests,
    -- even though we create the watcher after this.
    --
    -- On Windows, we occasionally see a test flake when there's no pause here.
    --
    -- So, let's put a healthy sleep between the setup actions and the watcher initialization.
    --
    -- When polling, we want to ensure we wait at least as long as the effective filesystem modification
    -- time granularity (which on Linux can be on the order of 10 milliseconds), *or*
    -- the poll interval, whichever is greater.
    threadDelay (max 5_000_000 (3 * pollInterval))

    let conf = defaultConfig {
#ifdef OS_BSD
          confWatchMode = if poll then WatchModePoll pollInterval else error "No native watcher available."
#else
          confWatchMode = if poll then WatchModePoll pollInterval else WatchModeOS
#endif
          , confThreadingMode = threadingMode
          }

    withRunInIO $ \runInIO ->
      withManagerConf conf $ \mgr -> do
        eventsVar <- newIORef []
        bracket
          (watchFn mgr watchedDir' (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ()))))
          (\stop -> stop)
          (\_ -> runInIO $ action $ TestFolderContext {
            watchedDir = watchedDir'
            , filePath = p
            , getEvents = readIORef eventsVar
            , clearEvents = atomicWriteIORef eventsVar []
            }
          )

parallelWithoutDirectory :: SpecFree context m () -> SpecFree context m ()
parallelWithoutDirectory = parallel' (defaultNodeOptions {
                                         nodeOptionsCreateFolder = False
                                         , nodeOptionsVisibilityThreshold = 70
                                         })
