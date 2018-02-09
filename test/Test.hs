{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO.Error
import System.IO.Temp
import System.PosixCompat.Files
import System.Random as R
import Test.Tasty
import Test.Tasty.HUnit

import EventUtils

isMac :: Bool
#ifdef darwin_HOST_OS
isMac = True
#else
isMac = False
#endif

isWindows :: Bool
#ifdef mingw32_HOST_OS
isWindows = True
#else
isWindows = False
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
  defaultMain $ withResource (createDirectoryIfMissing True testDirPath)
                             (const $ removeDirectoryRecursive testDirPath)
                             (const $ tests hasNative)

-- | There's some kind of race here in OS X where the creation of the containing directory shows up as an event
-- I explored whether this was due to passing 0 as the sinceWhen argument to FSEventStreamCreate
-- in the hfsevents package, but changing that didn't seem to help
pauseBeforeStartingTest :: IO ()
pauseBeforeStartingTest = threadDelay 10000

tests :: Bool -> TestTree
tests hasNative = testGroup "Tests" $ do
  poll <- if hasNative then [False, True] else [True]
  let ?timeInterval = if poll then 2*10^(6 :: Int) else 5*10^(5 :: Int)

  return $ testGroup (if poll then "Polling" else "Native") $ do
    recursive <- [False, True]
    return $ testGroup (if recursive then "Recursive" else "Non-recursive") $ do
      nested <- [False, True]

      let pollDelay = when poll (threadDelay $ 10^(6 :: Int))

      return $ testGroup (if nested then "In a subdirectory" else "Right here") $ do
        t <- [ mkTest "new file" (if | poll -> [evAdded False]
                                     | isWindows -> [evAdded False]
                                     | isMac -> [evAddedOrModified False]
                                     | otherwise -> [evAdded False, evModified False])
                                 (const $ return ())
                                 (\f -> writeFile f "foo")

             , mkTest "modify file" [evModified False]
                                    (\f -> writeFile f "")
                                    (\f -> when poll (threadDelay $ 10^(6 :: Int)) >> appendFile f "foo")

             , mkTest "delete file" [evRemoved False]
                                    (\f -> writeFile f "")
                                    (\f -> removeFile f)

             , mkTest "new directory" (if | isMac -> [evAddedOrModified True]
                                          | otherwise -> [evAdded True])
                                      (const $ return ())
                                      (\f -> createDirectory f)

             , mkTest "delete directory" [evRemoved True]
                                         (\f -> pollDelay >> createDirectory f)
                                         (\f -> removeDirectory f)
          ]
        return $ t nested recursive poll


mkTest :: (?timeInterval::Int) => TestName -> [FilePath -> EventPattern] -> (FilePath -> IO a) ->
          (FilePath -> IO ()) -> Bool -> Bool -> Bool -> TestTree
mkTest title evs prepare action nested recursive poll = do
  testCase title $ do
    -- Use a random identifier so that every test happens in a different folder
    -- This is unfortunately necessary because of the madness of OS X FSEvents; see the comments in OSX.hs
    randomID <- replicateM 10 $ R.randomRIO ('a', 'z')

    withTempDirectory testDirPath ("test." <> randomID) $ \watchedDir -> do
      let fileName = "testfile"
      let baseDir = if nested then watchedDir </> "subdir" else watchedDir
          f = normalise $ baseDir </> fileName
          watchFn = if recursive then watchTree else watchDir
          expect = expectEvents poll watchFn watchedDir

      createDirectoryIfMissing True baseDir

      pauseBeforeStartingTest

      flip finally (isFile f >>= flip when (removeFile f)) $ do
        _ <- prepare f
        pauseBeforeStartingTest
        flip expect (action f) (if | nested && (not recursive) -> []
                                   | otherwise -> map ($ f) evs)




-------------------------------------------------------------------------------
isFile :: FilePath -> IO Bool
isFile p = handleJust h return checkFile
  where
    h e = if isDoesNotExistError e
          then Just False
          else Nothing
    checkFile = isRegularFile <$> getFileStatus p
