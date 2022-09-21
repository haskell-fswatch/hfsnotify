{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module FSNotify.Test.Util where

import Control.Exception.Safe (Handler(..))
import Control.Monad
import Control.Retry
import System.FSNotify
import System.FilePath
import System.PosixCompat.Files (touchFile)
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

pauseAndRetryOnExpectationFailure :: (MonadUnliftIO m, ?timeInterval :: Int) => Int -> m a -> m a
pauseAndRetryOnExpectationFailure n action = threadDelay ?timeInterval >> retryOnExpectationFailure n action

retryOnExpectationFailure :: MonadUnliftIO m => Int -> m a -> m a
#if MIN_VERSION_retry(0, 7, 0)
retryOnExpectationFailure seconds action = withRunInIO $ \runInIO -> recovering (constantDelay 50000 <> limitRetries (seconds * 20)) [\_ -> Handler handleFn] (\_ -> runInIO action)
#else
retryOnExpectationFailure seconds action = withRunInIO $ \runInIO -> recovering (constantDelay 50000 <> limitRetries (seconds * 20)) [\_ -> Handler handleFn] (runInIO action)
#endif
  where
    handleFn :: SomeException -> IO Bool
    handleFn (fromException -> Just (Reason {})) = return True
    handleFn _ = return False


data TestFolderContext = TestFolderContext {
  watchedDir :: FilePath
  , filePath :: FilePath
  , getEvents :: IO [Event]
  , clearEvents :: IO ()
  }

testFolderContext :: Label "testFolderContext" TestFolderContext
testFolderContext = Label :: Label "testFolderContext" TestFolderContext

introduceTestFolder :: (MonadUnliftIO m, ?timeInterval :: Int) => ThreadingMode -> Bool -> Bool -> Bool -> SpecFree (LabelValue "testFolderContext" TestFolderContext :> context) m () -> SpecFree context m ()
introduceTestFolder threadingMode poll recursive nested = introduceWith "Make test folder" testFolderContext $ \action -> do
  withRandomTempDirectory $ \watchedDir' -> do
    let fileName = "testfile"
    let baseDir = if nested then watchedDir' </> "subdir" else watchedDir'
    let watchFn = if recursive then watchTree else watchDir

    createDirectoryIfMissing True baseDir

    -- On Mac, delay before starting the watcher because otherwise creation of "subdir"
    -- can get picked up.
    when isMac $ threadDelay 2000000

    let conf = defaultConfig {
          confWatchMode = if poll then WatchModePoll (2 * 10^(5 :: Int)) else WatchModeOS
          , confThreadingMode = threadingMode
          }

    withRunInIO $ \runInIO ->
      withManagerConf conf $ \mgr -> do
        eventsVar <- newIORef []
        stop <- watchFn mgr watchedDir' (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ())))
        _ <- runInIO $ action $ TestFolderContext {
          watchedDir = watchedDir'
          , filePath = normalise $ baseDir </> fileName
          , getEvents = readIORef eventsVar
          , clearEvents = threadDelay ?timeInterval >> atomicWriteIORef eventsVar []
          }

        stop


-- | Use a random identifier so that every test happens in a different folder
-- This is unfortunately necessary because of the madness of OS X FSEvents; see the comments in OSX.hs
withRandomTempDirectory :: MonadUnliftIO m => (FilePath -> m ()) -> m ()
withRandomTempDirectory action = do
  randomID <- replicateM 10 $ R.randomRIO ('a', 'z')
  withSystemTempDirectory ("test." <> randomID) action

withParallelSemaphore :: forall context m. (
  MonadUnliftIO m, HasParallelSemaphore context
  ) => SpecFree context m () -> SpecFree context m ()
withParallelSemaphore = around' (defaultNodeOptions { nodeOptionsRecordTime = False, nodeOptionsVisibilityThreshold = 125 }) "claim semaphore" $ \action -> do
  s <- getContext parallelSemaphore
  bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void action)
