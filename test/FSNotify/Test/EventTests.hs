{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FSNotify.Test.EventTests where

import Control.Exception.Safe
import Control.Monad
import Data.Monoid
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO
import Test.Hspec


eventTests :: ThreadingMode -> Spec
eventTests threadingMode = describe "Tests" $
  forM_ [False, True] $ \poll -> describe (if poll then "Polling" else "Native") $ do
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
            h <- openFile f AppendMode

            flip finally (hClose h) $
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

            withFile f WriteMode $ \h ->
              flip finally (hClose h) $ do
                hPutStr h "foo"
                pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
                  if | nested && not recursive -> events `shouldBe` []
                     | otherwise -> case events of
#ifdef darwin_HOST_OS
                         [ModifiedAttributes {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
#else
                         [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
#endif
                         _ -> expectationFailure $ "Got wrong events: " <> show events <> " (wanted file path " <> show f <> ")"

#ifdef linux_HOST_OS
          unless poll $
            it "gets a close_write" $ \(_watchedDir, f, getEvents, clearEvents) -> do
              writeFile f "" >> clearEvents
              withFile f WriteMode $ flip hPutStr "asdf"
              pauseAndRetryOnExpectationFailure 3 $ getEvents >>= \events ->
                if | nested && not recursive -> events `shouldBe` []
                   | otherwise -> case events of
                       [cw@(CloseWrite {}), m@(Modified {})]
                         | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                           && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
                       [m@(Modified {}), cw@(CloseWrite {})]
                         | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                           && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
                       _ -> expectationFailure $ "Got wrong events: " <> show events
#endif
