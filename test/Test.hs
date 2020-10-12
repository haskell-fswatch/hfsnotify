{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf, LambdaCase, RecordWildCards, ViewPatterns #-}

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Catch
import Control.Retry
import Data.IORef
import Data.Monoid
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO
import System.IO.Temp
import System.PosixCompat.Files
import System.Random as R
import Test.HUnit.Lang
import Test.Hspec


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

main :: IO ()
main = do
  hasNative <- nativeMgrSupported
  unless hasNative $ putStrLn "WARNING: native manager cannot be used or tested on this platform"
  hspec $ do
    describe "SingleThread" $ tests SingleThread hasNative
    describe "ThreadPerWatch" $ tests ThreadPerWatch hasNative
    describe "ThreadPerEvent" $ tests ThreadPerEvent hasNative

tests :: ThreadingMode -> Bool -> Spec
tests threadingMode hasNative = describe "Tests" $
  forM_ (if hasNative then [False, True] else [True]) $ \poll -> describe (if poll then "Polling" else "Native") $ do
    let ?timeInterval = if poll then 2*10^(6 :: Int) else 5*10^(5 :: Int)
    forM_ [False, True] $ \recursive -> describe (if recursive then "Recursive" else "Non-recursive") $
      forM_ [False, True] $ \nested -> describe (if nested then "In a subdirectory" else "Right here") $
        makeTestFolder threadingMode poll recursive nested $ do
          unless (nested || poll || isMac || isWin) $ it "deletes the watched directory" $ \(watchedDir, _f, getEvents, _clearEvents) -> do
            removeDirectory watchedDir

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \case
              [WatchedDirectoryRemoved {..}] | eventPath `equalFilePath` watchedDir && eventIsDirectory == IsDirectory -> return ()
              events -> expectationFailure $ "Got wrong events: " <> show events

          it "works with a new file" $ \(_watchedDir, f, getEvents, _clearEvents) -> do
            openFile f AppendMode >>= hClose

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
                     [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
                     _ -> expectationFailure $ "Got wrong events: " <> show events

          it "works with a new directory" $ \(_watchedDir, f, getEvents, _clearEvents) -> do
            createDirectory f

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
                     [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
                     _ -> expectationFailure $ "Got wrong events: " <> show events

          it "works with a deleted file" $ \(_watchedDir, f, getEvents, clearEvents) -> do
            writeFile f "" >> clearEvents

            removeFile f

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
                     [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
                     _ -> expectationFailure $ "Got wrong events: " <> show events

          it "works with a deleted directory" $ \(_watchedDir, f, getEvents, clearEvents) -> do
            createDirectory f >> clearEvents

            removeDirectory f

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
                     [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
                     _ -> expectationFailure $ "Got wrong events: " <> show events

          it "works with modified file attributes" $ \(_watchedDir, f, getEvents, clearEvents) -> do
            writeFile f "" >> clearEvents

            changeFileAttributes f

            -- This test is disabled when polling because the PollManager only keeps track of
            -- modification time, so it won't catch an unrelated file attribute change
            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | poll -> return ()
                 | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
#ifdef mingw32_HOST_OS
                     [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
#else
                     [ModifiedAttributes {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
#endif
                     _ -> expectationFailure $ "Got wrong events: " <> show events

          it "works with a modified file" $ \(_watchedDir, f, getEvents, clearEvents) -> do
            writeFile f "" >> clearEvents

            appendFile f "foo"

            pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
              if | nested && not recursive -> events `shouldBe` []
                 | otherwise -> case events of
                     [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
                     _ -> expectationFailure $ "Got wrong events: " <> show events



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
  -- Use a random identifier so that every test happens in a different folder
  -- This is unfortunately necessary because of the madness of OS X FSEvents; see the comments in OSX.hs
  randomID <- replicateM 10 $ R.randomRIO ('a', 'z')

  withSystemTempDirectory ("test." <> randomID) $ \watchedDir -> do
    let fileName = "testfile"
    let baseDir = if nested then watchedDir </> "subdir" else watchedDir
    let watchFn = if recursive then watchTree else watchDir

    createDirectoryIfMissing True baseDir

    -- On Mac, delay before starting the watcher because otherwise creation of "subdir"
    -- can get picked up.
    when isMac $ threadDelay 2000000

    mgr <- startManagerConf defaultConfig {
      confDebounce = NoDebounce
      , confWatchMode = if poll then WatchModePoll (2 * 10^(5 :: Int)) else WatchModeOS
      , confThreadingMode = threadingMode
      }
    eventsVar <- newIORef []
    stop <- watchFn mgr watchedDir (const True) (\ev -> atomicModifyIORef eventsVar (\evs -> (ev:evs, ())))
    let clearEvents = threadDelay ?timeInterval >> atomicWriteIORef eventsVar []
    _ <- action (watchedDir, normalise $ baseDir </> fileName, readIORef eventsVar, clearEvents)
    stop
