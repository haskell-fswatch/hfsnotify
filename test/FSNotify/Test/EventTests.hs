{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module FSNotify.Test.EventTests where

import Control.Exception.Safe (MonadThrow)
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.FSNotify
import System.FilePath
import System.IO (hPutStr)
import Test.Sandwich
import UnliftIO hiding (poll)
import UnliftIO.Directory


eventTests :: (
  MonadUnliftIO m, MonadThrow m
  ) => ThreadingMode -> SpecFree context m ()
eventTests threadingMode = describe "Tests" $ parallelWithoutDirectory $ do
  let pollOptions = if isBSD then [True] else [False, True]

  forM_ pollOptions $ \poll -> describe (if poll then "Polling" else "Native") $ parallelWithoutDirectory $ do
    forM_ [False, True] $ \recursive -> describe (if recursive then "Recursive" else "Non-recursive") $ parallelWithoutDirectory $
      forM_ [False, True] $ \nested -> describe (if nested then "Nested" else "Non-nested") $ parallelWithoutDirectory $
        eventTests' threadingMode poll recursive nested

eventTests' :: (
  MonadUnliftIO m, MonadThrow m
  ) => ThreadingMode -> Bool -> Bool -> Bool -> SpecFree context m ()
eventTests' threadingMode poll recursive nested = do
  let withFolder' = withTestFolder threadingMode poll recursive nested
  let withFolder = withFolder' (const $ return ())
  let waitForEvents getEvents action = waitUntil 5.0 (liftIO getEvents >>= action)

  unless (nested || poll || isMac || isWin) $ it "deletes the watched directory" $ withFolder $ \(TestFolderContext watchedDir _f getEvents _clearEvents) -> do
    removeDirectory watchedDir

    waitForEvents getEvents $ \case
      [WatchedDirectoryRemoved {..}] | eventPath `equalFilePath` watchedDir && eventIsDirectory == IsDirectory -> return ()
      events -> expectationFailure $ "Got wrong events: " <> show events

  it "works with a new file" $ withFolder $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    let wrapper action = if | isWin -> liftIO (writeFile f "foo") >> action
                            | otherwise -> withFile f AppendMode $ \_ -> action

    wrapper $
      waitForEvents getEvents $ \events ->
        if | nested && not recursive -> events `shouldBe` []
           | isWin && not poll -> case events of
               -- On Windows, we sometimes get an extra modified event
               [Modified {}, Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               [Added {..}, Modified {}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events
           | otherwise -> case events of
               [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events

  it "works with a new directory" $ withFolder $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    createDirectory f

    waitForEvents getEvents $ \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  it "works with a deleted file" $ withFolder' (\f -> liftIO $ writeFile f "") $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    removeFile f

    waitForEvents getEvents $ \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  it "works with a deleted directory" $ withFolder' (\f -> liftIO $ createDirectory f) $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    removeDirectory f

    waitForEvents getEvents $ \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  it "works with modified file attributes" $ withFolder' (\f -> liftIO $ writeFile f "") $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    liftIO $ changeFileAttributes f

    -- This test is disabled when polling because the PollManager only keeps track of
    -- modification time, so it won't catch an unrelated file attribute change
    waitForEvents getEvents $ \events ->
      if | poll -> return ()
         | nested && not recursive -> events `shouldBe` []
         | isWin -> case events of
             [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events
         | otherwise -> case events of
             [ModifiedAttributes {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  it "works with a modified file" $ withFolder' (\f -> liftIO $ writeFile f "") $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
    (if isWin then withSingleWriteFile f "foo" else withOpenWritableAndWrite f "foo") $
      waitForEvents getEvents $ \events ->
        if | nested && not recursive -> events `shouldBe` []
           | isMac -> case events of
               [Modified {..}] | poll && eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               [ModifiedAttributes {..}] | not poll && eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events <> " (wanted file path " <> show f <> ")"
           | otherwise -> case events of
               [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events <> " (wanted file path " <> show f <> ")"

  when isLinux $ unless poll $
    it "gets a close_write" $ withFolder' (\f -> liftIO $ writeFile f "") $ \(TestFolderContext _watchedDir f getEvents _clearEvents) -> do
      liftIO $ withFile f WriteMode $ flip hPutStr "asdf"
      waitForEvents getEvents $ \events ->
        if | nested && not recursive -> events `shouldBe` []
           | otherwise -> case events of
               [cw@(CloseWrite {}), m@(Modified {})]
                 | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                   && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
               [m@(Modified {}), cw@(CloseWrite {})]
                 | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                   && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events

withSingleWriteFile :: MonadIO m => FilePath -> String -> m b -> m b
withSingleWriteFile fp contents action = do
  liftIO $ writeFile fp contents
  action

withOpenWritableAndWrite :: MonadUnliftIO m => FilePath -> String -> m b -> m b
withOpenWritableAndWrite fp contents action = do
  withFile fp WriteMode $ \h ->
    flip finally (hClose h) $ do
      liftIO $ hPutStr h contents
      action
