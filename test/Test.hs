{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams, MultiWayIf #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO.Error
import System.IO.Temp
import System.PosixCompat.Files
import Test.Tasty
import Test.Tasty.HUnit

import EventUtils

data OS = Mac | Windows | Linux | Unknown deriving (Eq, Show)

os :: OS
#ifdef OS_Linux
os = Linux
#else
#  ifdef OS_Win32
os = Windows
#  else
#    ifdef OS_Mac
os = Mac
#    else
os = Unknown
#    endif
#  endif
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

-- Dummy main for playing with individual tests
main' :: IO ()
main' = do
  let poll = False
  let ?timeInterval = 2000000
  -- let ?timeInterval = if poll then 2*10^(6 :: Int) else 5*10^(5 :: Int)

  let tests' = [mkTest "new file" [evModified]
                                  (\f -> writeFile f "asdf" >> threadDelay 100000)
                                  (\f -> appendFile f "foo")
                                  False -- Nested
                                  False -- Recursive
                                  poll -- Poll
               ]

  defaultMain $ withResource (createDirectoryIfMissing True testDirPath)
                             (const $ removeDirectoryRecursive testDirPath)
                             (const $ (testGroup "Single test" tests'))


-- | There's some kind of race here in OS X where the creation of the directory shows up as an event
-- Putting a short sleep here seems to fix it
-- I explored whether this was due to passing 0 as the sinceWhen argument to FSEventStreamCreate
-- in the hfsevents package, but changing that didn't seem to help
pauseBeforeStartingTest :: IO ()
pauseBeforeStartingTest = threadDelay 10000


tests :: Bool -> TestTree
tests hasNative = testGroup "Tests" $ do
  -- poll <- if hasNative then [False, True] else [True]
  poll <- if hasNative then [False] else [True]
  let ?timeInterval = if poll then 2*10^(6 :: Int) else 5*10^(5 :: Int)

  return $ testGroup (if poll then "Polling" else "Native") $ do
    recursive <- [False, True]
    return $ testGroup (if recursive then "Recursive" else "Non-recursive") $ do
      nested <- [False, True]

      let pollDelay = when poll (threadDelay $ 10^(6 :: Int))

      return $ testGroup (if nested then "In a subdirectory" else "Right here") $ do
        t <- [ mkTest "new file" (if | poll -> [evAdded]
                                     | os == Mac -> [evAddedOrModified]
                                     | otherwise -> [evAdded, evModified])
                                 (const $ return ())
                                 (\f -> writeFile f "foo")

             , mkTest "modify file" [evModified]
                                    (\f -> writeFile f "")
                                    (\f -> when poll (threadDelay $ 10^(6 :: Int)) >> appendFile f "foo")

             , mkTest "delete file" [evRemoved]
                                    (\f -> writeFile f "")
                                    (\f -> removeFile f)

             , mkTest "new directory" [evAdded]
                                      (const $ return ())
                                      (\f -> createDirectory f)

             , mkTest "delete directory" [evRemoved]
                                         (\f -> pollDelay >> createDirectory f)
                                         (\f -> removeDirectory f)
          ]
        return $ t nested recursive poll


mkTest :: (?timeInterval::Int) => TestName -> [FilePath -> EventPattern] -> (FilePath -> IO a) ->
          (FilePath -> IO ()) -> Bool -> Bool -> Bool -> TestTree
mkTest title evs prepare action nested recursive poll =
  testCase title $ withTempDirectory testDirPath "test." $ \watchedDir -> do
    let fileName = "testfile"
    let baseDir = if nested then watchedDir </> "subdir" else watchedDir
        f = baseDir </> fileName
        watchFn = if recursive then watchTree else watchDir
        expect = expectEvents poll watchFn watchedDir

    createDirectoryIfMissing True baseDir

    pauseBeforeStartingTest

    flip finally (isFile f >>= flip when (removeFile f)) $ do
      _ <- prepare f
      pauseBeforeStartingTest
      expect (if not nested || recursive then map ($ f) evs else []) (action f)




-------------------------------------------------------------------------------
isFile :: FilePath -> IO Bool
isFile p = handleJust h return checkFile
  where
    h e = if isDoesNotExistError e
          then Just False
          else Nothing
    checkFile = isRegularFile <$> getFileStatus p
