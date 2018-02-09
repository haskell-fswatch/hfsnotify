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
import System.IO
import System.IO.Error
import System.IO.Temp
import System.PosixCompat.Files
import System.Random as R
import Test.Tasty
import Test.Tasty.HUnit

import EventUtils

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

-- | There's some kind of race in OS X where the creation of the containing directory shows up as an event
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
        t <- [ mkTest "new file" (if | isMac && not poll -> [evAddedOrModified False]
                                     | otherwise -> [evAdded False])
                                 (const $ return ())
                                 (\f -> openFile f AppendMode >>= hClose)

             , mkTest "modify file" [evModified False]
                                    (\f -> writeFile f "")
                                    (\f -> pollDelay >> appendFile f "foo")

             -- This test is disabled when polling because the PollManager only keeps track of
             -- modification time, so it won't catch an unrelated file attribute change
             , mkTest "modify file attributes" (if poll then [] else [evModified False])
                                               (\f -> writeFile f "")
                                               (\f -> if poll then return () else changeFileAttributes f)

             , mkTest "delete file" [evRemoved False]
                                    (\f -> writeFile f "")
                                    removeFile

             , mkTest "new directory" (if | isMac -> [evAddedOrModified True]
                                          | otherwise -> [evAdded True])
                                      (const $ return ())
                                      createDirectory

             , mkTest "delete directory" [evRemoved True]
                                         (\f -> pollDelay >> createDirectory f)
                                         removeDirectory
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
                                   | otherwise -> [ev f | ev <- evs])




-------------------------------------------------------------------------------
isFile :: FilePath -> IO Bool
isFile p = handleJust h return checkFile
  where
    h e = if isDoesNotExistError e
          then Just False
          else Nothing
    checkFile = isRegularFile <$> getFileStatus p
