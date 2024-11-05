{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module FSNotify.Test.Util where

import Control.Exception.Safe (Handler(..))
import Control.Monad
import Control.Monad.Logger
import Control.Retry
import Data.String.Interpolate
import System.FSNotify
import System.FilePath
import System.Random as R
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

withTestFolder :: (
  MonadUnliftIO m, MonadLogger m
  )
  => ThreadingMode
  -> Bool
  -> Bool
  -> Bool
  -> (FilePath -> m ())
  -> (TestFolderContext -> m a)
  -> m a
withTestFolder threadingMode poll recursive nested setup action = do
  withRandomTempDirectory $ \watchedDir' -> do
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
    -- When polling, we want to ensure we wait at least as long as the effective filesystem modification
    -- time granularity (which on Linux can be on the order of 10 milliseconds), *or*
    -- the poll interval, whichever is greater.
    when (isMac || poll) $ threadDelay (max 1000000 pollInterval)

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

-- | Use a random identifier so that every test happens in a different folder
-- This is unfortunately necessary because of the madness of OS X FSEvents; see the comments in OSX.hs
withRandomTempDirectory :: MonadUnliftIO m => (FilePath -> m a) -> m a
withRandomTempDirectory action = do
  randomID <- liftIO $ replicateM 10 $ R.randomRIO ('a', 'z')
  withSystemTempDirectory ("test." <> randomID) action

parallelWithoutDirectory :: SpecFree context m () -> SpecFree context m ()
parallelWithoutDirectory = parallel' (defaultNodeOptions {
                                         nodeOptionsCreateFolder = False
                                         , nodeOptionsVisibilityThreshold = 70
                                         })

-- withParallelSemaphore :: forall context m. (
--   MonadUnliftIO m, HasLabel context "parallelSemaphore" QSem
--   ) => SpecFree context m () -> SpecFree context m ()
-- withParallelSemaphore = around' (defaultNodeOptions { nodeOptionsRecordTime = False, nodeOptionsVisibilityThreshold = 125 }) "claim semaphore" $ \action -> do
--   s <- getContext parallelSemaphore'
--   bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)

-- parallelSemaphore' :: Label "parallelSemaphore" QSem
-- parallelSemaphore' = Label

-- type HasParallelSemaphore' context = HasLabel context "parallelSemaphore" QSem
